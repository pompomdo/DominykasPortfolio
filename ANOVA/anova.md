# ANOVA Model:

A statistical model to test hypothesis that number of visuals of products influences their return rate.

### 1. Data
    ```{r}
    anovaV1_4 <- as.data.frame(read.csv("return_reasons_201911_202071.csv"))

    anovaV1_4[is.na(anovaV1_4)] <- 0 #NA into 0s (on all columns)

    library(dplyr)

    anovaV1_4 <- filter_at(anovaV1_4, vars(visualcount), all_vars((.) != 0))

    ```

    ```{r}
    library(dplyr)

    anovaV1_4_dplyred <- anovaV1_4 %>%
      group_by(product_name) %>%
      summarize(
        QS = sum(gross_quantity_sold),
        QR = sum(returned_quantity),
        VC = max(visualcount),
        PB = max(sale_price)
                     )

    ```

    ```{r}
    anovaV1_4_dplyred <- cbind(anovaV1_4_dplyred,return_rate = anovaV1_4_dplyred$QR / anovaV1_4_dplyred$QS )

    is.na(anovaV1_4_dplyred)<-sapply(anovaV1_4_dplyred, is.infinite)
    anovaV1_4_dplyred[is.na(anovaV1_4_dplyred)]<-0

    ```

    ```{r}
    First_mean <- mean(anovaV1_4_dplyred$return_rate)
    First_mean
    ```

    ```{r}
    anovaV1_4 <- anovaV1_4_dplyred[,c(-1,-2,-3)] # excluding unnecessary variables

    str(anovaV1_4)
    summary(anovaV1_4)

    ```

#### 1.1. Data inspection
    ```{r}
    library(tidyverse)

    ggplot(data = anovaV1_4, mapping = aes(x=PB)) +
      geom_histogram(aes(y=..density..),fill="bisque",color="white",alpha=0.7) +
      geom_density() +
      geom_rug() +
      labs(x='mean sale price') +
      theme_minimal()
    ```

    ```{r}
    ggplot(data = anovaV1_4, mapping = aes(x=VC)) +
      geom_histogram(aes(y=..density..),fill="bisque",color="white",alpha=0.7) +
      geom_density() +
      geom_rug() +
      labs(x='mean number of visuals') +
      theme_minimal()
    ```



#### 1.2. Tagging data 1


    ```{r}

    tags <- c("[0-10)","[10-20)", "[20-30)", "[30-40)", "[40-50)","[50-60)","[60-70)","[70-80)",
              "[80-90)","[90-100)","[100-150)","[150-300)")


    anovaV1_4 <- as_tibble(anovaV1_4) %>%  ## BUCKETS
      mutate(tag = case_when(
        PB < 10 ~ tags[1],
        PB >= 10 & PB < 20 ~ tags[2],
        PB >= 20 & PB < 30 ~ tags[3],
        PB >= 30 & PB < 40 ~ tags[4],
        PB >= 40 & PB < 50 ~ tags[5],
        PB >= 50 & PB < 60 ~ tags[6],
        PB >= 60 & PB < 70 ~ tags[7],
        PB >= 70 & PB < 80 ~ tags[8],
        PB >= 80 & PB < 90 ~ tags[9],
        PB >= 90 & PB < 100 ~ tags[10],
        PB >= 100 & PB < 150 ~ tags[11],
        PB >= 150 & PB < 300 ~ tags[12]
        ))

    summary(anovaV1_4)

    ```


#### 1.4. Factoring

    ```{r}

    anovaV1_4$VC <- ordered(anovaV1_4$VC,
                             levels = c( 1, 2, 3, 4, 5, 6, 7, 8,9,10,11,12,13,14,15),
                             exclude="NA")

    levels(anovaV1_4$VC) <- c( 1, 2, 3, 4, 5, 6, 7, "8+","8+","8+","8+","8+","8+","8+","8+")

    summary(anovaV1_4$VC)


    anovaV1_4$tag <- factor(anovaV1_4$tag,
                           levels = tags,
                           ordered = FALSE,
                           exclude="NA")


    summary(anovaV1_4$tag)

    ```

#### 1.5. Data wrangling


    ```{r}
    anovaV1_4 <- subset(anovaV1_4, VC != "NA" & tag != "NA",
    select=c(return_rate, VC, tag))

    Second_mean <- mean(anovaV1_4$return_rate)
    Second_mean
    ```


#### 1.6. Two way ANOVA test no interaction

    ```{r}
    res.aov5 <- aov(return_rate ~ VC + tag, data = anovaV1_4)
    summary(res.aov5)

    ```

#### 1.6.1 Two way ANOVA test with interaction

    ```{r}
    res.aov5_1 <- aov(return_rate ~ VC * tag, data = anovaV1_4)
    summary(res.aov5_1)
    ```

#### 1.6.2 Assumptions

    ```{r}
    plot(res.aov5, 3)
    ```

    ```{r}
    aov_residuals <- residuals(object = res.aov5[1:4999])
    # Run Shapiro-Wilk test
    shapiro.test(x = aov_residuals[1:4999])
    ```

    ```{r}
    library(car)
    leveneTest(return_rate ~ VC * tag, data = anovaV1_4)
    ```

    ```{r}
    library(car)
    my_anova <- aov(return_rate ~ VC * tag, data = anovaV1_4)
    Anova(my_anova, type = "III")
    ```



#### 1.7. Summ stats

    ```{r}
    require("dplyr")
    for_table_results <- group_by(anovaV1_4, tag) %>%
      summarise(
        count = n(),
        mean = mean(return_rate, na.rm = TRUE),
        sd = sd(return_rate, na.rm = TRUE)
      )

    view(for_table_results)

    xyz <- data.frame(for_table_results)
    xyz
    ```

#### 1.7.1

    ```{r}
    TukeyHSD(res.aov5_1)
    ```


## 1.8. Visual

    ```{r}
    library("ggpubr")

    ggboxplot(anovaV1_4, x = "VC", y = "return_rate",color='tag')
    ```

## 1.8.1. Visual 2

    ```{r}
    library("ggpubr")
    ggline(anovaV1_4, x = "VC", y = "return_rate", color ='tag',
           add = "mean_se", xlab="Number of Visuals", ylab= "Return Rate")
    ```

    ```{r}
    library(granovaGG)
    granovagg.1w(data=anovaV1_4$return_rate, group=anovaV1_4$VC,
                 h.rng=1,
                 print.squares = T)
    ```

    ```{r}
    write.csv(xyz,"C:\\Users\\ddomi_000\\Desktop\\Otrium\\AnovaModel\\ANOVA.csv", row.names = FALSE)
    ```
