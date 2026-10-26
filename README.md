# jjstat

`jjstat`은 논문 작성과 통계 결과 보고에 자주 사용하는 분석 절차를 간편하게 수행하기 위한 R 패키지입니다. 기술통계, 결측값 처리, 신뢰도와 타당도, 평균 비교, 회귀분석, CFA/SEM, PLS-PM, 다층모형 및 잠재프로파일 분석을 위한 함수와 논문용 결과표 작성 기능을 제공합니다.

> 현재 CRAN 등록을 준비하고 있는 개발 버전입니다. CRAN 심사 전까지 함수명, 인수 또는 반환 형식이 변경될 수 있습니다.

## 제작자

**박중희**  
연세대학교 심리학과 겸임교수  
인지공학 박사

- GitHub: <https://github.com/shoutjoy/jjstat>
- 문의: <shoutjoy@hanmail.net>

## 주요 특징

- 논문 작성에 필요한 기술통계와 APA 형식 결과표 생성
- 결측값 확인과 대체
- Cronbach's alpha, CR, AVE, HTMT, ICC 계산
- t 검정, 분산분석, 카이제곱 검정과 사후분석
- 회귀분석과 매개·조절효과 정리
- `lavaan` 기반 CFA와 구조방정식모형 결과 정리
- `plspm` 기반 PLS-PM 분석, 효과량 및 모형 시각화
- 다층모형, 잠재계층 및 잠재프로파일 분석 보조
- 데이터 변환, 재코딩, 표 구성과 그래프 작성

## 설치

### GitHub 개발 버전

```r
install.packages("remotes")
remotes::install_github(
  "shoutjoy/jjstat",
  dependencies = TRUE,
  force = TRUE
)
```

설치 후 패키지를 불러옵니다.

```r
library(jjstat)
```

### 로컬 소스에서 설치

저장소를 내려받은 경우 다음과 같이 설치할 수 있습니다.

```r
devtools::install(
  "F:/Rwork/SEM212/jjstat",
  dependencies = TRUE
)
```

## 빠른 사용 예제

### 결측값 확인

`missCheck()`는 변수별 관측값 수와 결측값 수를 반환합니다. `plot = TRUE`를 사용하려면 선택 패키지인 `VIM`이 필요합니다.

```r
library(jjstat)

dat <- data.frame(
  score1 = c(10, 12, NA, 15),
  score2 = c(8, NA, 11, 14),
  group  = c("A", "A", "B", "B")
)

missCheck(dat)
```

결측값을 평균으로 대체하려면 다음과 같이 실행합니다.

```r
filled <- missfillin(
  dat[c("score1", "score2")],
  fn = mean
)

filled
```

`mice`의 예측평균대체(PMM)를 사용하려면 다음과 같이 실행합니다.

```r
if (requireNamespace("mice", quietly = TRUE)) {
  filled_pmm <- missfillin(
    dat[c("score1", "score2")],
    method = "pmm"
  )
}
```

### 공분산행렬과 상관행렬 변환

```r
vars <- mtcars[c("mpg", "hp", "wt")]

cov_mat <- cov(vars)
cor_mat <- Cov2cor(cov_mat)

cor_mat

restored_cov <- Cor2cov(
  cor_mat,
  sds = vapply(vars, stats::sd, numeric(1))
)

restored_cov
```

하삼각행렬을 문자열로 입력해 전체 행렬을 만들 수도 있습니다.

```r
cor_mat <- GetCov(
  "1
   .30 1
   .20 .40 1",
  names = c("x1", "x2", "x3")
)

cor_mat
```

### 빈도분석

```r
Freq_table(
  mtcars$cyl,
  prop = TRUE
)
```

그래프를 함께 출력하려면 `plot = TRUE`를 사용합니다.

```r
Freq_table(
  mtcars$cyl,
  prop = TRUE,
  plot = TRUE,
  xlab = "Number of cylinders"
)
```

### 신뢰도 분석

상관행렬 또는 `lavaan` 적합 객체를 이용해 신뢰도를 계산할 수 있습니다.

```r
item_cor <- cor(mtcars[c("mpg", "disp", "hp", "wt")])

cronbach_alpha(item_cor)
```

### 회귀분석 결과 정리

```r
fit <- lm(mpg ~ wt + hp, data = mtcars)

reg_apa(fit)
```

### 확인적 요인분석

```r
model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"

fit <- lavaan::cfa(
  model,
  data = lavaan::HolzingerSwineford1939
)

summary(fit, fit.measures = TRUE, standardized = TRUE)
```

`cfa2()`, `sem_fit()`, `summary_sem()` 및 `sem_plot()`을 사용하면 적합도, 표준화 계수 및 모형 그림을 논문 작성에 적합한 형태로 정리할 수 있습니다. 그래프 함수에는 `semPlot`, `semptools` 등의 선택 패키지가 필요할 수 있습니다.

## 주요 함수 목록

아래 표는 처음 사용할 때 유용한 대표 함수를 기능별로 정리한 것입니다. 자세한 인수와 반환값은 `?함수명`으로 확인할 수 있습니다.

### 기술통계와 빈도분석

| 함수 | 설명 |
|---|---|
| `DescribeBy()` | 집단별 기술통계를 계산합니다. |
| `mysummary()` | 변수별 요약통계를 생성합니다. |
| `mysummaryBy()` | 집단을 기준으로 요약통계를 생성합니다. |
| `mysummary_apa()` | 요약통계를 APA 보고 형식으로 정리합니다. |
| `Freq_table()` | 빈도와 비율을 계산하고 선택적으로 그래프를 그립니다. |
| `Freq_table_apa()` | 빈도분석 결과를 논문용 표로 정리합니다. |
| `Freq_table_multi()` | 여러 변수의 빈도표를 한 번에 생성합니다. |
| `mean_apa()` | 평균과 관련 통계를 APA 형식으로 정리합니다. |
| `SKEW()` | 왜도를 계산합니다. |
| `KURT()` | 첨도를 계산합니다. |
| `Normality_test()` | 여러 변수의 정규성 검정을 수행합니다. |
| `shapiro_test_all()` | 여러 변수에 Shapiro-Wilk 검정을 적용합니다. |
| `histogram()` | 분포 확인용 히스토그램을 생성합니다. |

### 결측값과 데이터 전처리

| 함수 | 설명 |
|---|---|
| `missCheck()` | 변수별 결측값과 관측값 수를 확인합니다. |
| `missfillin()` | 전체 데이터의 결측값을 평균, 중앙값 또는 PMM으로 대체합니다. |
| `missfillinFor()` | 선택한 열 범위의 결측값만 대체합니다. |
| `na2zero()` | 결측값을 0으로 변경합니다. |
| `all_na_zero()` | 모든 값이 결측인 경우를 확인하거나 0으로 처리합니다. |
| `Char2num()` | 문자형 값을 숫자형으로 변환합니다. |
| `binary_recode()` | 변수를 이분형 값으로 재코딩합니다. |
| `rev_coding()` | 역문항을 재코딩합니다. |
| `centering()` | 연속변수를 중심화합니다. |
| `meanCentering_df()` | 데이터프레임의 여러 변수를 평균중심화합니다. |
| `convert_scale_df()` | 여러 변수의 척도를 변환합니다. |
| `min_max_scaler()` | 최소-최대 정규화를 수행합니다. |
| `to_long()` | 데이터를 long 형식으로 변환합니다. |
| `to_wide()` | 데이터를 wide 형식으로 변환합니다. |

### 신뢰도와 타당도

| 함수 | 설명 |
|---|---|
| `cronbach_alpha()` | 상관행렬 또는 `lavaan` 객체에서 Cronbach's alpha를 계산합니다. |
| `cronbach_from_cor()` | 상관행렬로 Cronbach's alpha를 계산합니다. |
| `alpha_apa()` | 신뢰도 분석 결과를 APA 형식으로 정리합니다. |
| `alpha_table()` | 여러 척도의 신뢰도 결과표를 만듭니다. |
| `item_CR_AVE()` | 문항 적재량을 이용해 CR과 AVE를 계산합니다. |
| `cr_ave_from_lambda()` | 표준화 요인적재량으로 CR과 AVE를 계산합니다. |
| `HTMT()` | `lavaan` 모형의 HTMT 판별타당도를 계산합니다. |
| `htmt_sem()` | SEM 측정모형의 HTMT 결과를 정리합니다. |
| `ICC()` | 혼합모형 자료의 급내상관계수를 계산합니다. |
| `icc_lme4()` | `lme4` 모형에서 ICC를 계산합니다. |
| `indicator_validity_ml()` | 다층 측정모형의 지표 타당도를 확인합니다. |

### 상관, 공분산 및 요인분석

| 함수 | 설명 |
|---|---|
| `Cov2cor()` | 공분산행렬을 상관행렬로 변환합니다. |
| `Cor2cov()` | 상관행렬과 표준편차로 공분산행렬을 만듭니다. |
| `GetCov()` | 하삼각 또는 상삼각 입력으로 전체 행렬을 생성합니다. |
| `add_cor()` | 데이터 또는 결과표에 상관계수를 추가합니다. |
| `cov_simdata()` | 공분산구조를 이용한 모의자료 생성을 지원합니다. |
| `bartlett_test()` | Bartlett 구형성 검정을 수행합니다. |
| `summary_kmo()` | KMO 결과를 정리합니다. |
| `summary_bartlett()` | Bartlett 검정 결과를 정리합니다. |
| `summary_fa()` | 요인분석 결과와 설명분산을 표로 정리합니다. |
| `factor_scores()` | 요인점수를 계산하거나 데이터에 추가합니다. |
| `item_parceling()` | 문항묶음 점수를 생성합니다. |

### 평균 비교와 범주형 자료 분석

| 함수 | 설명 |
|---|---|
| `t_test_report()` | t 검정 결과를 논문 보고 형식으로 정리합니다. |
| `ttest_df()` | 여러 변수의 t 검정 결과를 데이터프레임으로 반환합니다. |
| `oneway_aov()` | 일원분산분석과 결과표를 생성합니다. |
| `aov_apa()` | 분산분석 결과를 APA 형식으로 정리합니다. |
| `aov_post()` | 분산분석 사후검정을 수행합니다. |
| `aov_df_emmeans()` | `emmeans` 기반 평균 비교 결과를 정리합니다. |
| `report_aov()` | 분산분석 결과의 문장형 보고를 지원합니다. |
| `chisq_test()` | 독립성 또는 적합도 카이제곱 검정을 수행합니다. |
| `chisq_apa()` | 카이제곱 검정 결과를 APA 형식으로 정리합니다. |
| `chisq_posthoc()` | 카이제곱 검정의 사후 셀 비교를 수행합니다. |
| `cramers_v()` | 범주형 변수 간 효과크기 Cramer's V를 계산합니다. |
| `mcnemar_test()` | 대응 범주형 자료에 McNemar 검정을 수행합니다. |
| `cmh_test()` | Cochran-Mantel-Haenszel 검정을 수행합니다. |

### 회귀, 매개효과 및 조절효과

| 함수 | 설명 |
|---|---|
| `reg_apa()` | 선형회귀 결과를 APA 형식으로 정리합니다. |
| `cal_vif()` | 회귀모형의 분산팽창요인(VIF)을 계산합니다. |
| `white_se()` | 이분산성에 강건한 표준오차를 계산합니다. |
| `Sobel_test()` | 간접효과에 대한 Sobel 검정을 수행합니다. |
| `sobel_test_serial()` | 연속 다중매개효과의 Sobel 검정을 지원합니다. |
| `med_effect()` | 매개효과를 계산하고 정리합니다. |
| `ModSEM()` | 구조방정식 기반 조절효과 분석을 지원합니다. |
| `DE_effect()` | 직접효과를 추출하거나 계산합니다. |
| `IE_effect()` | 간접효과를 추출하거나 계산합니다. |
| `TE_effect()` | 총효과를 추출하거나 계산합니다. |
| `Diff_effect()` | 두 효과의 차이를 계산합니다. |
| `confint_plot_lm()` | 회귀계수의 신뢰구간을 시각화합니다. |

### CFA와 구조방정식모형

| 함수 | 설명 |
|---|---|
| `cfa2()` | CFA 적합도, 적재량, 신뢰도 및 타당도 결과를 정리합니다. |
| `cfa_ml()` | 확인적 요인분석 실행을 지원합니다. |
| `jjstat_SEM()` | SEM 분석과 결과 정리를 통합적으로 수행합니다. |
| `SEM()` | 구조방정식모형 분석을 실행하는 보조 함수입니다. |
| `sem_fit()` | SEM 적합도 지수를 정리합니다. |
| `summary_sem()` | SEM 결과의 주요 통계를 요약합니다. |
| `sem_apa()` | SEM 경로 결과를 APA 형식으로 정리합니다. |
| `sem_effect()` | 직접·간접·총효과를 정리합니다. |
| `sem_effect_ci()` | SEM 효과의 신뢰구간을 정리합니다. |
| `sem_plot()` | SEM 경로도를 생성합니다. |
| `LavaanPlot()` | `lavaan` 적합 객체를 경로도로 시각화합니다. |
| `Modindices()` | 수정지수를 추출하고 정리합니다. |
| `lav_modelfit()` | `lavaan` 모형 적합도 지수를 반환합니다. |
| `lav_extract_path()` | `lavaan` 결과에서 구조경로를 추출합니다. |
| `lav_extract_ind()` | 간접효과 경로를 추출합니다. |
| `lav_semPaths()` | `lavaan` 모형의 경로도를 작성합니다. |

### PLS-PM

| 함수 | 설명 |
|---|---|
| `plspm_sem()` | 자료, 경로행렬 및 블록으로 PLS-PM 분석을 실행합니다. |
| `plspm_modelfit()` | PLS-PM 모형 적합도를 정리합니다. |
| `plspm_CRAVE()` | PLS-PM의 CR과 AVE를 계산합니다. |
| `plspm_htmt()` | PLS-PM의 HTMT를 계산합니다. |
| `plspm_loadings()` | 외부모형의 요인적재량을 정리합니다. |
| `plspm_crossloadings()` | 교차적재량을 정리합니다. |
| `plspm_paths()` | 내부모형의 경로계수를 정리합니다. |
| `plspm_effect()` | PLS-PM의 직접·간접·총효과를 계산합니다. |
| `plspm_f2()` | 경로별 효과크기 f²를 계산합니다. |
| `plspm_boot_summary()` | 부트스트랩 결과를 요약합니다. |
| `plspm_multigroups()` | PLS-PM 다집단 비교를 지원합니다. |
| `plspm_permutation_test()` | 집단 차이에 대한 순열검정을 수행합니다. |
| `plspm_model_plot()` | PLS-PM 모형을 시각화합니다. |

### 다층모형과 잠재집단 분석

| 함수 | 설명 |
|---|---|
| `lme_report()` | 선형혼합모형 결과를 논문용으로 정리합니다. |
| `report_lme()` | 혼합모형 결과의 문장형 보고를 지원합니다. |
| `bind_lme_model()` | 여러 혼합모형 결과를 결합합니다. |
| `mlsem_fit_indices()` | 다층 SEM의 적합도 지수를 정리합니다. |
| `mlsem_reliability()` | 다층 측정모형의 신뢰도를 계산합니다. |
| `mlsem_htmt()` | 다층 SEM의 HTMT를 계산합니다. |
| `lpa()` | 잠재프로파일 분석을 실행합니다. |
| `lpa_create_profiles()` | 잠재프로파일 후보모형을 생성합니다. |
| `lpa_explore_modelfit()` | 잠재프로파일 모형 적합도를 비교합니다. |
| `lpa_profile_plot()` | 잠재프로파일의 평균 패턴을 시각화합니다. |
| `lca_result()` | 잠재계층분석 결과를 정리합니다. |
| `lca_class_plot()` | 잠재계층별 패턴을 시각화합니다. |

### 결과표와 시각화

| 함수 | 설명 |
|---|---|
| `table_apa()` | 결과표를 APA 형식으로 정리합니다. |
| `markdown_table()` | Markdown 형식의 표를 생성합니다. |
| `nice_table()` | 가독성 높은 결과표를 만듭니다. |
| `add_sig()` | p 값에 유의성 기호를 추가합니다. |
| `p_mark_sig()` | p 값에 따라 유의성 표시를 생성합니다. |
| `format_number()` | 숫자와 소수점 표시를 일관되게 맞춥니다. |
| `comma()` | 큰 숫자에 천 단위 구분기호를 추가합니다. |
| `mean_bar()` | 집단 평균 막대그래프를 생성합니다. |
| `compare_bar()` | 두 개 이상의 결과를 막대그래프로 비교합니다. |
| `plot_pie_chart()` | 범주형 자료의 원그래프를 생성합니다. |
| `confint_plot_data()` | 신뢰구간 그래프용 데이터를 구성합니다. |

## 전체 공개 함수 색인

현재 `NAMESPACE`에는 546개의 공개 함수가 등록되어 있습니다. 아래 목록은 함수명을 알파벳순으로 정리한 것입니다. CRAN 준비 과정에서 중복 기능과 내부 보조 함수는 정리될 수 있습니다.

<details>
<summary>전체 함수 목록 펼치기</summary>

`accent_table`, `add_bind_stat`, `add_col_ratio`, `add_cor`, `add_id`, `add_normality`, `add_normality_kline`, `add_partial_sum`  
`add_pca_scores`, `add_prop`, `add_ratio`, `add_ratio_df`, `add_rep_id`, `add_row_sum`, `add_rows`, `add_rows_freq`  
`add_sig`, `add_sig_star`, `add_significance_symbols`, `add_stat`, `add_sum`, `add_t_sig`, `all_na_zero`, `all_na_zero1`  
`alpha_apa`, `alpha_table`, `anova_apa`, `aov_apa`, `aov_df`, `aov_df_emmeans`, `aov_df2`, `aov_post`  
`aov_post2`, `aov_rename`, `aov_table`, `apa_aov_df`, `apa_aov_df_res`, `aroian_sobel_test_general`, `arrange_by_mind`, `as_trt`  
`as_trt2`, `auto_coding`, `ave_candidates_plot`, `ave_raise_candidates`, `bartlett_test`, `binary_recode`, `binary_recode_sep`, `bind_alpha_table`  
`bind_compare_data`, `bind_lme_model`, `bind_mysummaryBy`, `box_cox`, `breslowdata_test`, `cal_vif`, `calculate_chi_sig`, `centering`  
`cfa_ml`, `cfa2`, `change_node_name`, `Char2num`, `Char2num_fct`, `chisq.multcomp`, `chisq_apa`, `chisq_contrast`  
`chisq_gof_posthoc`, `chisq_gof_posthoc_apa`, `chisq_multcomp2`, `chisq_pivot_table`, `chisq_posthoc`, `chisq_retest`, `chisq_table_sum`, `chisq_test`  
`chisq_test_By1`, `chisq_test_By2`, `chisq_test_By3`, `chisq_test_cell`, `chisq_test_cor`, `chisq_test_kge`, `chisqTest`, `cmh_test`  
`col_capital`, `col_to_colnames`, `col_to_rownames`, `col2row`, `collapse_path`, `collapse_path2`, `colnames_by_dict`, `colnames_change`  
`colnames_change_df`, `colnames_to_row`, `combine_data`, `combine_rows`, `comma`, `compare_bar`, `compare_bind_data`, `confint_mark_sig`  
`confint_plot_data`, `confint_plot_lm`, `confint_plot_sem`, `contrast_msdn`, `convert_min_max_scaler`, `convert_scale_df`, `Cor2cov`, `count_ne`  
`cov_lm_sem`, `cov_simdata`, `Cov2cor`, `cr_ave_from_lambda`, `cr_candidates_plot`, `cr_raise_candidates`, `cramers_v`, `cronbach_alpha`  
`cronbach_from_cor`, `cut_print`, `cut_print_all`, `dall`, `datapaste`, `datapaste_text`, `DE_effect`, `DescribeBy`  
`diagram`, `diagram_model`, `diagram2`, `Diff_effect`, `e`, `EAI_index`, `edit_layout`, `effect1`  
`fa_print`, `factor_layout`, `factor_scores`, `fei_Chisq_test`, `fei_dv_res`, `filter_rows`, `find_paths`, `find_paths_cal`  
`format_col`, `format_number`, `Freq_rowcounter`, `Freq_table`, `Freq_table_apa`, `Freq_table_colnames`, `Freq_table_multi`, `fs`  
`g`, `general_Sobel_test`, `GetCov`, `goodman_sobel_test_general`, `gtl`, `gtl_df`, `hist2`, `histogram`  
`HTMT`, `htmt_mlsem`, `htmt_sem`, `ICC`, `icc_batch`, `icc_lme4`, `IE_effect`, `indicator_validity_ml`  
`interlist`, `interplot`, `interplot_data`, `interpretation_de`, `interpretation_ie`, `interpretation_ie_CI`, `item`, `item_CR_AVE`  
`item_parceling`, `jjstat_plot_path`, `jjstat_SEM`, `jtl`, `k`, `k_df`, `kakaoi`, `kge_chisq_table`  
`kge_weigth`, `kge_weigth_add_ca`, `kge_weigth_ko`, `KURT`, `lav_change_node_name`, `lav_de_hypo`, `lav_edge_value`, `lav_extract_imlist`  
`lav_extract_ind`, `lav_extract_ind_paths`, `lav_extract_int`, `lav_extract_mm`, `lav_extract_mm2`, `lav_extract_path`, `lav_extract_sm`, `lav_factor_layout`  
`lav_htmt`, `lav_ind_hypo`, `lav_interact_model`, `lav_latentProd`, `lav_matrix`, `lav_modelfit`, `lav_new_model`, `lav_Pattern`  
`lav_remodel`, `lav_return_blocks`, `lav_return_data`, `lav_return_model`, `lav_rotate_resid`, `lav_semPaths`, `lav_semPaths2`, `LavaanPlot`  
`layout_mat`, `lca_class_add`, `lca_class_freq`, `lca_class_plot`, `lca_freq_plot`, `lca_gamma`, `lca_gof`, `lca_GraphData`  
`lca_result`, `lcaGraphData`, `lme_report`, `long_df`, `lowerdf`, `lowerMat`, `lowerMat_na`, `lpa`  
`lpa_BIC_plot`, `lpa_bind_classdata`, `lpa_bind_classdata_tidyLPA`, `lpa_create_profiles`, `lpa_create_profiles2`, `lpa_explore_modelfit`, `lpa_gmm`, `lpa_plot`  
`lpa_profile_plot`, `lpa_profile_plot_summary`, `lpa_profile_plot2`, `LRT_apa`, `LRT_apa2`, `make_c_text`, `make_df_text`, `make_label_list`  
`make_layout`, `make_list_text`, `make_mat_text`, `make_mat_text_col`, `make_mat_text2`, `make_text_df`, `markdown_table`, `mat_text_arrange`  
`mcnemar_test`, `md`, `mean_apa`, `mean_bar`, `meanCentering`, `meanCentering_df`, `med_effect`, `metacorMat`  
`min_max_scaler`, `missCheck`, `missfillin`, `missfillinFor`, `mlsem_fit_indices`, `mlsem_fornell_larcker`, `mlsem_htmt`, `mlsem_modelfit_paper`  
`mlsem_reliability`, `modelfit_paper`, `Modindices`, `ModSEM`, `move_col`, `move_cols`, `move_mat`, `move_mat_change`  
`move_mat_cut`, `move_mat_nc`, `move_mat_ori`, `move_mat_substr`, `move_matpos`, `move_row`, `move_rows`, `mplus_est_df`  
`mplus_fit_df`, `mr`, `mrs`, `msdn_contrast`, `msdn_ttest`, `mutate_col`, `mydes`, `mysummary`  
`mysummary_apa`, `mysummaryBy`, `na_remover`, `na2zero`, `nice_aov_df`, `nice_table`, `nice_Var`, `Normality_test`  
`obs_exp_table`, `oneway_aov`, `p_mark_sig`, `p_sig_cal`, `p_to_001`, `p_value_cal`, `pall`, `path`  
`pattern_detector`, `patternGraph`, `patternGraph_obs_exp`, `patternGraph_obs_exp_ko`, `patternGraph2`, `pcfa`, `pcfa_porp_plot`, `pdata`  
`perform_chi_square_test`, `pivot_table`, `plot_lpa`, `plot_pie_chart`, `pls_modelfit_apa`, `plspm_add_blocks`, `plspm_add_inter_paths`, `plspm_apa_boot_paths`  
`plspm_apa_grp`, `plspm_apa_ind`, `plspm_apa_paths`, `plspm_auto_blocks`, `plspm_auto_genData`, `plspm_blocks`, `plspm_blocks_match`, `plspm_blocks2lav`  
`plspm_boot_effect`, `plspm_boot_factor_layout`, `plspm_boot_factor_point_to`, `plspm_boot_ind_factor`, `plspm_boot_ind_order`, `plspm_boot_ind_push`, `plspm_boot_paths_sig`, `plspm_boot_paths_sig_vec`  
`plspm_boot_summary`, `plspm_cfa`, `plspm_change_nodeLabels`, `plspm_col2lav`, `plspm_cor`, `plspm_CRAVE`, `plspm_crossloadings`, `plspm_crossloadings_plot`  
`plspm_curve`, `plspm_diagram_model`, `plspm_dULS_modelfit`, `plspm_edge_position`, `plspm_edge_values`, `plspm_edges_coefs`, `plspm_edges_loadings`, `plspm_edges_values`  
`plspm_effect`, `plspm_effect_bar`, `plspm_effectbar`, `plspm_extract_blocks`, `plspm_f2`, `plspm_f2_plot`, `plspm_factor_layout`, `plspm_fl`  
`plspm_grp_comp`, `plspm_grp_comp_apa`, `plspm_grp_compare_bar`, `plspm_grp_summary`, `plspm_htmt`, `plspm_htmt2`, `plspm_ind_effect`, `plspm_ind_effect_boot`  
`plspm_ind_effect_sig`, `plspm_ind_effect_test`, `plspm_ind_factor`, `plspm_ind_order`, `plspm_ind_order2`, `plspm_ind_push`, `plspm_ind_spread`, `plspm_indicator_factor`  
`plspm_indicator_order`, `plspm_indicator_test`, `plspm_indicator_test_multi`, `plspm_inner_model_sig`, `plspm_innerplot`, `plspm_interaction`, `plspm_lav2blocks`, `plspm_lav2paths`  
`plspm_layout_mat`, `plspm_loading_lav`, `plspm_loadings`, `plspm_loadings_plot`, `plspm_loadings_values`, `plspm_make_layout_mat`, `plspm_make_prod`, `plspm_mat_lav`  
`plspm_model_lav`, `plspm_model_plot`, `plspm_modelfit`, `plspm_multigroups`, `plspm_path_coefs_plot`, `plspm_paths`, `plspm_paths_coefs_plot`, `plspm_paths_sig`  
`plspm_paths_sig_plot`, `plspm_paths2lav`, `plspm_permutation_test`, `plspm_permutation_test_multi`, `plspm_prod`, `plspm_rotate_resid`, `plspm_sem`, `plspm_semPaths`  
`plspm_semPaths_lav`, `plspm_semPaths2`, `plspm_semptools`, `poolcor_comment`, `posthoc_duncan`, `posthoc_plot`, `PRE`, `Pull`  
`Pull_to_text`, `read_text`, `recode_sep_vec`, `reg_apa`, `remove_all_attr`, `remove_attr`, `remove_blank`, `remove_cols`  
`remove_filter`, `remove_rows`, `rename_by_dict`, `reorder_row`, `Replace`, `replace_dataframe`, `replace_df`, `replace_df_rep`  
`replace_df2`, `replace_in_dataframe`, `replace_in_df`, `Replace_na`, `report_aov`, `report_aov_ko`, `report_lme`, `report_manova`  
`report_stat`, `report_ttest`, `report_vartest`, `rev_coding`, `rev_coding_extract_only`, `rev_coding_map`, `rev_coding_plot_var`, `rev_coding_rep`  
`rev_coding_restore_original`, `rev_coding_validate_scale`, `revcode`, `Rload`, `Round`, `round_col`, `round2`, `row_filter`  
`row2col`, `row2cols`, `rowdata2col`, `rownames2colnames`, `rows_to_colnames`, `Rsave`, `Rsave_xlsx`, `save_xlsx`  
`select_format`, `SEM`, `sem_apa`, `sem_apa_ie`, `sem_apa_ie_CI`, `sem_APA_path_table`, `sem_count_df`, `sem_effect`  
`sem_effect_ci`, `sem_effect_diff`, `sem_effect_ie`, `sem_effect_ind`, `sem_effect_te`, `sem_fit`, `sem_lav_hypo`, `sem_lav_ind_hypo`  
`sem_model`, `sem_plot`, `semPaths2`, `shapiro_test_all`, `shapiro_test_df`, `simdata_cov`, `simdata_sem`, `simple_bar`  
`SKEW`, `Sobel_test`, `sobel_test_extend`, `sobel_test_general`, `sobel_test_serial`, `split_korean_word`, `split_korean_word_check`, `split_kw_df`  
`split_kw_df_match`, `split_word`, `str_cut`, `summary_bartlett`, `summary_fa`, `summary_kmo`, `summary_sem`, `t_sep`  
`t_test_report`, `table_apa`, `table_df`, `table_df_bar`, `table_df_bar2`, `table_df_colnames`, `table_df_multi`, `table_freq`  
`table_reorder`, `table_sum`, `TE_effect`, `text2df`, `to_long`, `to_table`, `to_wide`, `ttest_df`  
`unCombine`, `uncombine_data`, `unCount`, `Unite`, `unite_ci`, `Unite_col`, `Unite_confint`, `unite_rows`  
`validate_residual_cov`, `validate_variance`, `wald_test`, `wald_test_df`, `wald_test_gender`, `wald_test_general`, `wald_test_general_df`, `wald_test_general_mat`  
`wald_test_general_str`, `wald_test_lm`, `wald_test2`, `web`, `webr`, `white_se`, `wide_df`, `write_text`  
`ztest`, `ztest_df`

</details>

R 세션에서 현재 설치된 버전의 전체 공개 함수 목록을 확인하려면 다음을 실행합니다.

```r
sort(getNamespaceExports("jjstat"))
```

특정 함수의 도움말은 다음과 같이 확인합니다.

```r
?missCheck
?cronbach_alpha
?cfa2
?plspm_sem

help(package = "jjstat")
```

## 개발 및 CRAN 등록

CRAN 등록을 위한 단계별 작업은 [process.md](process.md)에 정리되어 있습니다. 주요 준비 작업은 다음과 같습니다.

- 공개 함수와 데이터셋 문서 보완
- 함수 내부 `library()` 호출과 의존성 정리
- 실행 가능한 examples와 vignette 작성
- Windows, Linux 및 macOS 검사
- `R CMD check --as-cran` 경고와 NOTE 해결

버그 제보나 기능 제안은 GitHub Issues를 이용해 주세요.

## 라이선스

이 패키지는 GPL-3 라이선스를 따릅니다.
