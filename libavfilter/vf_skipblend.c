/*
 * Copyright (c) 2012 Stefano Sabatini
 * Copyright (c) 2017 Matthias C. M. Troffaes
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file skipblend filter, based on the framestep filter
 */

#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "avfilter.h"
#include "internal.h"
#include "video.h"

typedef struct NullContext {
    const AVClass *class;
    int64_t frame_step;  ///< step size in frames
    int64_t frame_blend; ///< how many frames to blend on each step
    int nb_planes;       ///< number of planes in the pixel format
    int planewidth[4];   ///< width of each plane (after subsampling)
    int planeheight[4];  ///< height of each plane (after subsampling)
    uint32_t *data[4];   ///< buffer for blending input frames

    void (*blend_set)(AVFilterContext *ctx, AVFrame *in, int plane);
    void (*blend_add)(AVFilterContext *ctx, AVFrame *in, int plane);
    void (*blend_div)(AVFilterContext *ctx, AVFrame *in, int plane);
} SkipBlendContext;

#define OFFSET(x) offsetof(SkipBlendContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM

/* note: maximum value for blend must satisfy
   blend * max_pixel_value <= max_buffer_value
   where max_pixel_value is UINT16_MAX (support up to 16 bit formats)
   and max_buffer_value is UINT32_MAX (internal blend buffer uses 32 bits) */

static const AVOption skipblend_options[] = {
    { "step", "set frame step",  OFFSET(frame_step), AV_OPT_TYPE_INT64, {.i64=1}, 1, UINT16_MAX, FLAGS},
    { "blend", "number of frames to blend per step",  OFFSET(frame_blend), AV_OPT_TYPE_INT64, {.i64=UINT16_MAX}, 1, UINT16_MAX, FLAGS},
    { NULL },
};

AVFILTER_DEFINE_CLASS(skipblend);

#define DEFINE_BLEND(NAME, TYPE, DECL, EXPR)                                   \
static void blend_##NAME##_##TYPE(AVFilterContext *ctx, AVFrame *in, int plane)\
{                                                                              \
    SkipBlendContext *s = ctx->priv;                                           \
    DECL                                                                       \
    const int height = s->planeheight[plane];                                  \
    const int width  = s->planewidth[plane];                                   \
    const int stride = in->linesize[plane] / sizeof(TYPE);                     \
    TYPE *src = (TYPE *)in->data[plane];                                       \
    uint32_t *dst = s->data[plane];                                            \
    int y, x;                                                                  \
                                                                               \
    for (y = 0; y < height; y++) {                                             \
        for (x = 0; x < width; x++) {                                          \
            EXPR;                                                              \
        }                                                                      \
        src += stride;                                                         \
    }                                                                          \
}

#define SET_DECL
#define SET_EXPR *dst++ = src[x]
#define ADD_DECL
#define ADD_EXPR *dst++ += src[x]
#define DIV_DECL const int frame_blend = s->frame_blend;
#define DIV_EXPR src[x] = *dst++ / frame_blend

DEFINE_BLEND(set, uint8_t,  SET_DECL, SET_EXPR)
DEFINE_BLEND(set, uint16_t, SET_DECL, SET_EXPR)
DEFINE_BLEND(add, uint8_t,  ADD_DECL, ADD_EXPR)
DEFINE_BLEND(add, uint16_t, ADD_DECL, ADD_EXPR)
DEFINE_BLEND(div, uint8_t,  DIV_DECL, DIV_EXPR)
DEFINE_BLEND(div, uint16_t, DIV_DECL, DIV_EXPR)

#undef SET_DECL
#undef SET_EXPR
#undef ADD_DECL
#undef ADD_EXPR
#undef DIV_DECL
#undef DIV_EXPR
#undef DEFINE_BLEND

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFilterContext *ctx = inlink->dst;
    SkipBlendContext *s = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    AVFrame *out = NULL;
    int64_t frame_pos = inlink->frame_count_out % s->frame_step;
    int direct = 0;

    /* update destination frame buffer; we need to do this even if filter is
       disabled because buffer might be used for later frames when filter is
       re-enabled */
    if (!frame_pos) {
        /* copy first frame to destination frame buffer */
        for (int plane = 0; plane < s->nb_planes; plane++)
            s->blend_set(ctx, in, plane);
    } else if (frame_pos < s->frame_blend) {
        /* add current frame to destination frame buffer */
        for (int plane = 0; plane < s->nb_planes; plane++)
            s->blend_add(ctx, in, plane);
    }

    /* write frame */
    if (ctx->is_disabled) {
        /* filter is disabled, so pass input frame as is */
        return ff_filter_frame(outlink, in);
    } else if ((frame_pos + 1) == s->frame_blend) {
        /* filter is enabled, so write when all frames are blended */
        /* create a writable frame */
        if (av_frame_is_writable(in)) {
            direct = 1;
            out = in;
        } else {
            out = ff_get_video_buffer(outlink, outlink->w, outlink->h);
            if (!out) {
                av_frame_free(&in);
                return AVERROR(ENOMEM);
            }
            av_frame_copy_props(out, in);
        }
        /* finalize destination frame */
        for (int plane = 0; plane < s->nb_planes; plane++)
            s->blend_div(ctx, out, plane);
        /* free extra frame if created, and pass on output frame */
        if (!direct)
            av_frame_free(&in);
        return ff_filter_frame(outlink, out);
    } else {
        av_frame_free(&in);
        return 0;
    }
}

static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_YUV410P, AV_PIX_FMT_YUV411P,
        AV_PIX_FMT_YUV420P, AV_PIX_FMT_YUV422P,
        AV_PIX_FMT_YUV440P, AV_PIX_FMT_YUV444P,
        AV_PIX_FMT_YUVJ444P, AV_PIX_FMT_YUVJ440P,
        AV_PIX_FMT_YUVJ422P, AV_PIX_FMT_YUVJ420P,
        AV_PIX_FMT_YUVJ411P,
        AV_PIX_FMT_YUVA420P, AV_PIX_FMT_YUVA422P, AV_PIX_FMT_YUVA444P,
        AV_PIX_FMT_GBRP, AV_PIX_FMT_GBRAP, AV_PIX_FMT_GRAY8,
        AV_PIX_FMT_YUV420P16, AV_PIX_FMT_YUV422P16, AV_PIX_FMT_YUV444P16,
        AV_PIX_FMT_YUVA420P16, AV_PIX_FMT_YUVA422P16, AV_PIX_FMT_YUVA444P16,
        AV_PIX_FMT_GBRP16, AV_PIX_FMT_GRAY16,
        AV_PIX_FMT_NV12, AV_PIX_FMT_NV21,
        AV_PIX_FMT_NONE
    };
    AVFilterFormats *fmts_list = ff_make_format_list(pix_fmts);
    if (!fmts_list)
        return AVERROR(ENOMEM);
    return ff_set_common_formats(ctx, fmts_list);
}

static int config_input_props(AVFilterLink *inlink)
{
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);
    const AVFilterContext *ctx = inlink->dst;
    SkipBlendContext *s = ctx->priv;

    s->planewidth[0] = s->planewidth[3] = inlink->w;
    s->planewidth[1] = s->planewidth[2] = AV_CEIL_RSHIFT(inlink->w, desc->log2_chroma_w);
    s->planeheight[0] = s->planeheight[3] = inlink->h;
    s->planeheight[1] = s->planeheight[2] = AV_CEIL_RSHIFT(inlink->h, desc->log2_chroma_h);
    s->nb_planes = av_pix_fmt_count_planes(inlink->format);
    for (int plane = 0; plane < s->nb_planes; plane++) {
        const int planesize = s->planewidth[plane] * s->planeheight[plane];
        s->data[plane] = av_mallocz_array(planesize, sizeof(uint32_t));
        if (!s->data[plane])
            return AVERROR(ENOMEM);
    }
    if (desc->comp[0].depth == 8) {
        s->blend_set = blend_set_uint8_t;
        s->blend_add = blend_add_uint8_t;
        s->blend_div = blend_div_uint8_t;
    } else if (desc->comp[0].depth == 16) {
        s->blend_set = blend_set_uint16_t;
        s->blend_add = blend_add_uint16_t;
        s->blend_div = blend_div_uint16_t;
    } else {
        return AVERROR_BUG;
    }
    return 0;
}

static int config_output_props(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    const SkipBlendContext *s = ctx->priv;
    const AVFilterLink *inlink = ctx->inputs[0];

    outlink->frame_rate =
        av_div_q(inlink->frame_rate, (AVRational){s->frame_step, 1});

    av_log(ctx, AV_LOG_VERBOSE, "step:%"PRId64" frame_rate:%d/%d(%f) -> frame_rate:%d/%d(%f)\n",
           s->frame_step,
           inlink->frame_rate.num, inlink->frame_rate.den, av_q2d(inlink->frame_rate),
           outlink->frame_rate.num, outlink->frame_rate.den, av_q2d(outlink->frame_rate));

    return 0;
}

static av_cold int init(AVFilterContext *ctx)
{
    SkipBlendContext *s = ctx->priv;
    s->frame_blend = FFMIN(s->frame_blend, s->frame_step);
    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    SkipBlendContext *s = ctx->priv;
    for (int plane = 0; plane < s->nb_planes; plane++)
        av_freep(&s->data[plane]);
}

static const AVFilterPad skipblend_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .config_props = config_input_props,
        .filter_frame = filter_frame,
    },
    { NULL }
};

static const AVFilterPad skipblend_outputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .config_props = config_output_props,
    },
    { NULL }
};

AVFilter ff_vf_skipblend = {
    .name          = "skipblend",
    .description   = NULL_IF_CONFIG_SMALL("Skip frames whilst blending skipped frames."),
    .priv_size     = sizeof(SkipBlendContext),
    .priv_class    = &skipblend_class,
    .init          = init,
    .uninit        = uninit,
    .query_formats = query_formats,
    .inputs        = skipblend_inputs,
    .outputs       = skipblend_outputs,
    .flags         = AVFILTER_FLAG_SUPPORT_TIMELINE_INTERNAL,
};
