library(dplyr)
library(lubridate)
library(emmeans)
pos <- position_dodge(width = 0.3)

pl2 <- pl %>%
    mutate(
        DATE = factor(mdy(DATE)),
        trt_site = factor(trt_site),
        herbiv_log = log(X..HERBIVORY + 1)
    )

m_all <- lm(herbiv_log ~ trt_site * DATE, data = pl2)

emm_all <- emmeans(m_all, ~ trt_site * DATE)

cld_tbl <- cld(
    emm_all,
    adjust = "tukey",
    Letters = letters
) %>%
    as.data.frame() %>%
    mutate(
        letters = gsub(" ", "", .group)
    ) %>%
    select(DATE, trt_site, letters)

cld_tbl_new <- cld(
    emm_all,
    adjust = "tukey",
    Letters = letters,
    alpha = 0.1
) %>%
    as.data.frame() %>%
    mutate(
        letters = gsub(" ", "", .group)
    ) %>%
    select(DATE, trt_site, letters)

cld_tbl_new <- cld_tbl_new %>%
    mutate(
        DATE = as.Date(as.character(DATE))
    )
pl_plot <- pl_means %>%
    left_join(cld_tbl_new, by = c("DATE", "trt_site"))



ggplot(
    pl_plot,
    aes(
        x = factor(DATE),
        y = mean_herbiv_bt,
        color = trt_site
    )
) +
    geom_point(position = pos, size = 2) +
    geom_errorbar(
        aes(ymin = herbiv_lo, ymax = herbiv_hi),
        position = pos,
        width = 0.2,
        linewidth = 1
    ) +
    geom_text(
        aes(label = letters,
            y = herbiv_hi + 0.35),
        position = pos,
        size = 5,
        show.legend = FALSE
    ) +
    themeJG() +
    labs(
        x = "Date",
        y = "Mean percent herbivory (± SE)",
        color = "Site × Treatment"
    ) + theme(
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10)
) + theme(
    axis.title.x = element_text(margin = margin(t = 10))
)

fig_save("mean herbivory se1")

fig_save %>% .clip2()

anova(m_all)
