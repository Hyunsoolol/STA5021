## 1단계: 패키지 로드 & 작업 디렉토리 설정 ----
# 필요한 패키지 (없으면 설치)
# install.packages(c("readr", "dplyr", "lubridate", "ggplot2", "scales"))

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

# 작업 디렉토리 설정
setwd("C:/data/steam")

# 디렉토리 확인
list.files()

## 2단계: zip 내부 구조 확인 후 PoE만 추출 ----

# zip 안의 파일 목록 확인
zip_files <- unzip("PlayerCountHistoryPart1.zip", list = TRUE)
head(zip_files, 5)

# Path of Exile (appid 238960) 파일 찾기
poe_file <- zip_files[grepl("238960", zip_files$Name), ]
print(poe_file)

# 압축 해제할 폴더 생성
if (!dir.exists("extracted")) dir.create("extracted")

# PoE 파일만 추출
unzip("PlayerCountHistoryPart1.zip",
      files = poe_file$Name,
      exdir = "extracted/")

# 추출된 파일 경로 확인
extracted_path <- file.path("extracted", poe_file$Name)
print(extracted_path)
file.exists(extracted_path)

## 3단계: 데이터 로드 및 구조 확인 ----
# CSV 로드
poe_raw <- read_csv(extracted_path)

# 구조 확인
glimpse(poe_raw)
head(poe_raw, 5)
tail(poe_raw, 5)
nrow(poe_raw)


## 4단계: 시간 변환 + 일별 구조 생성 ----
poe <- poe_raw %>%
  rename(
    timestamp_utc = Time,
    players       = Playercount
  ) %>%
  mutate(
    timestamp_utc = force_tz(timestamp_utc, tzone = "UTC"),
    timestamp_kst = with_tz(timestamp_utc, "Asia/Seoul"),
    
    date_kst      = as.Date(timestamp_kst),
    hour_decimal  = hour(timestamp_kst) + minute(timestamp_kst) / 60,
    weekday       = lubridate::wday(date_kst, label = TRUE, week_start = 1),
    is_weekend    = lubridate::wday(date_kst, week_start = 1) %in% c(6, 7),
    
    log_players   = log1p(players)
  ) %>%
  arrange(timestamp_kst)

glimpse(poe)
range(poe$date_kst)
n_distinct(poe$date_kst)
summary(poe$players)


## 5단계: 결측/이상치 점검
# NA 분포
poe %>% summarise(
  n_total      = n(),
  n_NA_players = sum(is.na(players)),
  pct_NA       = round(mean(is.na(players)) * 100, 2)
)

# 시간 간격 점검 — 5분이 표준이어야 함
time_diffs <- diff(poe$timestamp_utc) %>% as.numeric(units = "mins")
cat("시간 간격 분포 (상위 10개):\n")
sort(table(round(time_diffs)), decreasing = TRUE)[1:10]

# 0인 관측치 (서버 점검 추정)
zero_obs <- poe %>% filter(players == 0)
cat("\nplayers == 0 인 관측치:", nrow(zero_obs), "\n")
head(zero_obs, 10)

# 일별 관측치 수
daily_count <- poe %>%
  group_by(date_kst) %>%
  summarise(
    n_obs       = n(),
    n_NA        = sum(is.na(players)),
    n_zero      = sum(players == 0, na.rm = TRUE),
    median_pl   = median(players, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n일별 관측치 수 요약:\n")
summary(daily_count$n_obs)

cat("\n완전한 날짜 (288개):", sum(daily_count$n_obs == 288), "\n")
cat("거의 완전 (≥280개):", sum(daily_count$n_obs >= 280), "\n")
cat("부족 (<280개):", sum(daily_count$n_obs < 280), "\n")

# 부족한 날짜
incomplete_days <- daily_count %>% 
  filter(n_obs < 280) %>% 
  arrange(n_obs)
print(incomplete_days, n = 30)

## 6단계: 전체 시계열 plot ----
ggplot(poe, aes(x = timestamp_kst, y = players)) +
  geom_line(color = "steelblue", alpha = 0.6) +
  scale_y_continuous(labels = comma) +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%Y-%m") +
  labs(title = "Path of Exile — Concurrent Players (2017–2020)",
       subtitle = "5-minute interval, KST",
       x = NULL, y = "Concurrent players") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 7단계: 0값과 NA 처리 ----
# 0과 NA를 함께 처리
poe <- poe %>%
  mutate(
    players_clean = if_else(is.na(players) | players == 0, NA_real_, players),
    log_players   = log1p(players_clean)
  )

# 정리 후 결측 분포
poe %>% summarise(
  n_total       = n(),
  n_NA_clean    = sum(is.na(players_clean)),
  pct_NA_clean  = round(mean(is.na(players_clean)) * 100, 2)
)

# 일별 결측 분포 — 어떤 날짜에 결측이 몰려있는지 확인
daily_na <- poe %>%
  group_by(date_kst) %>%
  summarise(
    n_NA  = sum(is.na(players_clean)),
    .groups = "drop"
  )

cat("\n일별 결측 분포:\n")
table(cut(daily_na$n_NA, breaks = c(-1, 0, 5, 20, 50, 288)))

# 결측이 많은 날짜 (50% 이상 NA = 144개 이상 NA)
heavily_missing <- daily_na %>% filter(n_NA > 144) %>% arrange(desc(n_NA))
cat("\n결측이 50% 이상인 날짜:\n")
print(heavily_missing)

## 8단계: 결측 많은 날짜 제외 + 최종 데이터 확정 ----
# 50% 이상 결측인 날짜는 분석에서 제외
bad_dates <- daily_na %>% filter(n_NA > 144) %>% pull(date_kst)
cat("제외할 날짜:", as.character(bad_dates), "\n")

poe <- poe %>%
  filter(!date_kst %in% bad_dates)

cat("최종 일수:", n_distinct(poe$date_kst), "\n")
cat("최종 관측치:", nrow(poe), "\n")

## 9단계: 함수형 데이터 행렬로 reshape ----
library(tidyr)

# Wide format으로 변환
X_mat <- poe %>%
  select(date_kst, hour_decimal, log_players) %>%
  pivot_wider(
    names_from  = hour_decimal,
    values_from = log_players,
    names_sort  = TRUE
  ) %>%
  arrange(date_kst)

# 차원 확인
cat("행렬 차원:", dim(X_mat), "\n")
# 첫 컬럼은 date_kst, 나머지 288개가 시간

# 행렬 부분만 추출 (행렬 형태로)
date_vec <- X_mat$date_kst
X_data   <- as.matrix(X_mat[, -1])
rownames(X_data) <- as.character(date_vec)

# 시간 grid
t_grid <- as.numeric(colnames(X_data))
cat("시간 grid 범위:", range(t_grid), "\n")
cat("시간 grid 길이:", length(t_grid), "\n")

# 첫 5행 일부 확인
X_data[1:5, 1:6]

## 10단계: 일별 곡선 spaghetti plot (첫 함수형 시각화) ----
# Long format으로 변환해서 plot
poe_func <- poe %>%
  filter(!is.na(log_players)) %>%
  select(date_kst, hour_decimal, log_players)

# 곡선 색상 = 시간 순서 (rainbow)
ggplot(poe_func, aes(x = hour_decimal, y = log_players, group = date_kst)) +
  geom_line(alpha = 0.05, color = "steelblue") +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  labs(title = "Path of Exile — Daily Intraday Curves",
       subtitle = paste0(n_distinct(poe$date_kst), " days, KST"),
       x = "Hour of day (KST)", y = "log(1 + players)") +
  theme_minimal()

## 11단계: 평균 곡선 (mean function) ----
# 시간대별 평균
mean_curve <- poe %>%
  filter(!is.na(log_players)) %>%
  group_by(hour_decimal) %>%
  summarise(
    mean_lp = mean(log_players),
    sd_lp   = sd(log_players),
    .groups = "drop"
  )

ggplot(mean_curve, aes(x = hour_decimal, y = mean_lp)) +
  geom_ribbon(aes(ymin = mean_lp - sd_lp, ymax = mean_lp + sd_lp),
              alpha = 0.3, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  labs(title = "Mean Daily Curve (KST) ± 1 SD",
       x = "Hour of day", y = "log(1 + players)") +
  theme_minimal()

## 12단계 (수정): UTC로 변환 ----
library(tidyr)

# UTC 기준으로 다시 정리
poe <- poe_raw %>%
  rename(
    timestamp_utc = Time,
    players       = Playercount
  ) %>%
  mutate(
    timestamp_utc = force_tz(timestamp_utc, tzone = "UTC"),
    
    # UTC 기준 날짜·시간
    date_utc      = as.Date(timestamp_utc),
    hour_decimal  = hour(timestamp_utc) + minute(timestamp_utc) / 60,
    weekday       = lubridate::wday(date_utc, label = TRUE, week_start = 1),
    is_weekend    = lubridate::wday(date_utc, week_start = 1) %in% c(6, 7),
    
    # NA + 0 처리
    players_clean = if_else(is.na(players) | players == 0, NA_real_, players),
    log_players   = log1p(players_clean)
  ) %>%
  arrange(timestamp_utc)

# 50% 이상 결측인 날짜 제외
daily_na <- poe %>%
  group_by(date_utc) %>%
  summarise(n_NA = sum(is.na(log_players)), .groups = "drop")

bad_dates <- daily_na %>% filter(n_NA > 144) %>% pull(date_utc)
cat("제외할 날짜:", as.character(bad_dates), "\n")

poe <- poe %>% filter(!date_utc %in% bad_dates)

cat("최종 일수:", n_distinct(poe$date_utc), "\n")
cat("최종 관측치:", nrow(poe), "\n")

## 13단계: 함수형 행렬로 reshape (UTC 기준) ----
X_mat <- poe %>%
  select(date_utc, hour_decimal, log_players) %>%
  pivot_wider(
    names_from  = hour_decimal,
    values_from = log_players,
    names_sort  = TRUE
  ) %>%
  arrange(date_utc)

# 행렬 형태로
date_vec <- X_mat$date_utc
X_data   <- as.matrix(X_mat[, -1])
rownames(X_data) <- as.character(date_vec)

t_grid <- as.numeric(colnames(X_data))

cat("행렬 차원:", dim(X_data), "\n")
cat("시간 grid 범위:", range(t_grid), "\n")

## 14단계: 새 spaghetti plot + mean curve (UTC) ----
# Spaghetti plot
poe_func <- poe %>%
  filter(!is.na(log_players)) %>%
  select(date_utc, hour_decimal, log_players)

p1 <- ggplot(poe_func, aes(x = hour_decimal, y = log_players, group = date_utc)) +
  geom_line(alpha = 0.05, color = "steelblue") +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  labs(title = "Path of Exile — Daily Intraday Curves (UTC)",
       subtitle = paste0(n_distinct(poe$date_utc), " days"),
       x = "Hour of day (UTC)", y = "log(1 + players)") +
  theme_minimal()
print(p1)

# Mean curve
mean_curve <- poe %>%
  filter(!is.na(log_players)) %>%
  group_by(hour_decimal) %>%
  summarise(
    mean_lp = mean(log_players),
    sd_lp   = sd(log_players),
    .groups = "drop"
  )

p2 <- ggplot(mean_curve, aes(x = hour_decimal, y = mean_lp)) +
  geom_ribbon(aes(ymin = mean_lp - sd_lp, ymax = mean_lp + sd_lp),
              alpha = 0.3, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  labs(title = "Mean Daily Curve (UTC) ± 1 SD",
       x = "Hour of day (UTC)", y = "log(1 + players)") +
  theme_minimal()
print(p2)

## 15단계: 결측 보간 ----
library(zoo)

# 행 단위 (각 날짜) 선형 보간
X_data_imp <- t(apply(X_data, 1, function(row) {
  na.approx(row, na.rm = FALSE, rule = 2)
}))

cat("보간 전 NA:", sum(is.na(X_data)), "\n")
cat("보간 후 NA:", sum(is.na(X_data_imp)), "\n")

# 결측이 있던 날짜 예시
example_idx <- which(rowSums(is.na(X_data)) > 30)[1]
example_date <- date_vec[example_idx]

plot(t_grid, X_data[example_idx, ], 
     type = "l", col = "red", lwd = 2,
     main = paste("Imputation example:", example_date),
     xlab = "Hour (UTC)", ylab = "log(1+players)")
lines(t_grid, X_data_imp[example_idx, ], col = "blue", lty = 2, lwd = 1.5)
legend("bottomleft", c("original (NA)", "imputed"), 
       col = c("red", "blue"), lty = c(1, 2), lwd = c(2, 1.5))

## 16단계: GCV로 최적 lambda 찾기 + B-spline smoothing ----
library(fda)

# B-spline basis
nbasis <- 51
bb <- create.bspline.basis(
  rangeval = c(0, 24),
  nbasis   = nbasis,
  norder   = 4
)

# 데이터 transpose: smooth.basis는 (시간 × 날짜) 형태 요구
X_t <- t(X_data_imp)   # 288 × 971

# GCV로 lambda 탐색
loglam     <- seq(-4, 4, by = 0.5)
gcv_values <- numeric(length(loglam))

for (i in seq_along(loglam)) {
  fdPar_i  <- fdPar(bb, Lfdobj = 2, lambda = 10^loglam[i])
  smooth_i <- smooth.basis(t_grid, X_t, fdPar_i)
  gcv_values[i] <- mean(smooth_i$gcv)
}

# GCV plot
plot(loglam, gcv_values, type = "b", pch = 19,
     xlab = "log10(lambda)", ylab = "Mean GCV",
     main = "GCV for smoothing parameter selection")
abline(v = loglam[which.min(gcv_values)], col = "red", lty = 2)

best_loglam <- loglam[which.min(gcv_values)]
cat("최적 log10(lambda):", best_loglam, "\n")
cat("최적 lambda:", 10^best_loglam, "\n")

## 17단계: 최적 lambda로 smoothing 후 시각화 ----
# 최적 lambda로 smoothing
fdPar_best <- fdPar(bb, Lfdobj = 2, lambda = 10^best_loglam)
poe_fd     <- smooth.basis(t_grid, X_t, fdPar_best)$fd

# 모든 곡선 plot
plot(poe_fd, col = scales::alpha("steelblue", 0.05),
     xlab = "Hour (UTC)", ylab = "log(1+players)",
     main = "Smoothed daily curves (UTC)",
     ylim = range(X_data_imp, na.rm = TRUE))

# 평균 함수 추가
mean_fd <- mean.fd(poe_fd)
lines(mean_fd, col = "red", lwd = 3)

# 분산 함수도
var_fd <- var.fd(poe_fd)
plot(var_fd, xlab = "Hour (UTC)", ylab = "Variance",
     main = "Variance function")

## 18단계 (수정): lambda 재탐색 (더 넓은 범위) ----
# 더 정밀하게 탐색 — 실용적인 범위로
loglam <- seq(-2, 2, by = 0.25)
gcv_values <- numeric(length(loglam))

for (i in seq_along(loglam)) {
  fdPar_i  <- fdPar(bb, Lfdobj = 2, lambda = 10^loglam[i])
  smooth_i <- smooth.basis(t_grid, X_t, fdPar_i)
  gcv_values[i] <- mean(smooth_i$gcv)
}

plot(loglam, gcv_values, type = "b", pch = 19,
     xlab = "log10(lambda)", ylab = "Mean GCV",
     main = "GCV (refined search)")
abline(v = loglam[which.min(gcv_values)], col = "red", lty = 2)

best_loglam <- loglam[which.min(gcv_values)]
cat("최적 log10(lambda):", best_loglam, "\n")
cat("최적 lambda:", 10^best_loglam, "\n")

## 19단계: 더 나은 smoothing + 시각화 (수정) ----
# Lambda 결정
# 옵션 1: GCV 최소
chosen_loglam <- best_loglam
# 옵션 2: 좀 더 강한 smoothing (mean curve 부드럽게)
# chosen_loglam <- 0   # lambda = 1

cat("선택한 log10(lambda):", chosen_loglam, "\n")

fdPar_best <- fdPar(bb, Lfdobj = 2, lambda = 10^chosen_loglam)
poe_fd     <- smooth.basis(t_grid, X_t, fdPar_best)$fd

# Smoothed curves plot (개선된 버전)
par(mfrow = c(1, 1))
plot(poe_fd, col = scales::alpha("steelblue", 0.05),
     xlab = "Hour (UTC)", ylab = "log(1+players)",
     main = "Smoothed daily curves (UTC)")
mean_fd <- mean.fd(poe_fd)
lines(mean_fd, col = "red", lwd = 3)

# Variance function 우회 — eval.bifd 사용
# var.fd는 bivariate fd object를 반환하므로 grid에서 평가해서 plot
var_bifd <- var.fd(poe_fd)

# 대각선 (variance function) 추출
var_diag <- diag(eval.bifd(t_grid, t_grid, var_bifd))

plot(t_grid, var_diag, type = "l", lwd = 2, col = "darkblue",
     xlab = "Hour (UTC)", ylab = "Variance",
     main = "Variance function")

## 20단계: 평균 ± SD 밴드로 시각화 ----
mean_vals <- eval.fd(t_grid, mean_fd)
sd_vals   <- sqrt(var_diag)

mean_df <- data.frame(
  hour = t_grid,
  mean = as.numeric(mean_vals),
  sd   = sd_vals
)

ggplot(mean_df, aes(x = hour, y = mean)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd),
              alpha = 0.3, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  labs(title = "Smoothed Mean Daily Curve (UTC) ± 1 SD",
       x = "Hour (UTC)", y = "log(1 + players)") +
  theme_minimal()

## 21단계: FPCA 실행 ----
# pca.fd로 FPCA 수행
poe_pca <- pca.fd(poe_fd, nharm = 4, 
                  harmfdPar = fdPar(bb, Lfdobj = 2, lambda = 1e-2))

# 분산 설명력
cat("FPC별 분산 설명 비율:\n")
print(round(poe_pca$varprop, 4))
cat("\n누적 분산 설명력:\n")
print(round(cumsum(poe_pca$varprop), 4))

## 22단계: FPC 시각화 ----
# 첫 4개 FPC 곡선
par(mfrow = c(2, 2))
for (k in 1:4) {
  plot(poe_pca$harmonics[k], 
       xlab = "Hour (UTC)", ylab = "FPC value",
       main = paste0("FPC ", k, 
                     " (", round(poe_pca$varprop[k] * 100, 1), "%)"))
  abline(h = 0, lty = 2, col = "gray")
}
par(mfrow = c(1, 1))

# Scree plot
plot(poe_pca$values[1:10], type = "b", pch = 19,
     xlab = "FPC index", ylab = "Eigenvalue",
     main = "Scree plot")

## 23단계: Mean ± FPC 시각화 (해석에 필수) ----
# Mean ± k * FPC 시각화
par(mfrow = c(2, 2))
for (k in 1:4) {
  mean_vals <- eval.fd(t_grid, mean_fd)
  fpc_vals  <- eval.fd(t_grid, poe_pca$harmonics[k])
  scale     <- 2 * sqrt(poe_pca$values[k])  # 2 SD
  
  plot(t_grid, mean_vals, type = "l", lwd = 2,
       ylim = range(c(mean_vals + scale * fpc_vals,
                      mean_vals - scale * fpc_vals)),
       xlab = "Hour (UTC)", ylab = "log(1+players)",
       main = paste0("FPC ", k, 
                     " (", round(poe_pca$varprop[k] * 100, 1), "%)"))
  lines(t_grid, mean_vals + scale * fpc_vals, col = "red", lwd = 2)
  lines(t_grid, mean_vals - scale * fpc_vals, col = "blue", lwd = 2)
  legend("topleft", c("mean", "+2SD", "-2SD"),
         col = c("black", "red", "blue"), lty = 1, lwd = 2, bty = "n")
}
par(mfrow = c(1, 1))

## 24단계: FPC scores 추출 ----
# FPC scores: 행 = 날짜, 열 = FPC
scores <- poe_pca$scores
colnames(scores) <- paste0("FPC", 1:ncol(scores))
rownames(scores) <- as.character(date_vec)

dim(scores)
head(scores)

# Score plot — FPC1 vs FPC2
plot(scores[, 1], scores[, 2],
     xlab = paste0("FPC1 (", round(poe_pca$varprop[1] * 100, 1), "%)"),
     ylab = paste0("FPC2 (", round(poe_pca$varprop[2] * 100, 1), "%)"),
     pch = 19, col = scales::alpha("steelblue", 0.4),
     main = "FPC scores: each point = one day")
abline(h = 0, v = 0, lty = 2, col = "gray")

## 25단계: FPC1 score를 시계열로 보기 (리그 효과 검증) ----
library(dplyr)
library(ggplot2)

scores_df <- data.frame(
  date = date_vec,
  FPC1 = scores[, 1],
  FPC2 = scores[, 2],
  FPC3 = scores[, 3],
  FPC4 = scores[, 4]
)

# FPC1 시계열 — 리그 spike 패턴이 보일 것
ggplot(scores_df, aes(x = date, y = FPC1)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  labs(title = "FPC1 score over time (overall daily level)",
       x = NULL, y = "FPC1 score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# FPC2 시계열 — 점검 outlier 보일 것
ggplot(scores_df, aes(x = date, y = FPC2)) +
  geom_line(color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  labs(title = "FPC2 score over time (evening peak modulation)",
       x = NULL, y = "FPC2 score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 26단계: FPC2 outlier 날짜 식별 ----
# FPC2 score가 크거나 작은 outlier 날짜 (절댓값 기준)
fpc2_outliers <- scores_df %>%
  arrange(desc(abs(FPC2))) %>%
  head(15)

print(fpc2_outliers)

# FPC1 outlier도 (가장 동접 많았던 날 / 적었던 날)
fpc1_top <- scores_df %>% arrange(desc(FPC1)) %>% head(10)
fpc1_bot <- scores_df %>% arrange(FPC1) %>% head(10)

cat("\n=== 동접 가장 많았던 10일 (FPC1 top) ===\n")
print(fpc1_top)

cat("\n=== 동접 가장 적었던 10일 (FPC1 bottom) ===\n")
print(fpc1_bot)

## 27단계: 리그 메타데이터 정리 ----
library(dplyr)

# PoE 리그 출시일 (Wikipedia 기준)
league_dates <- data.frame(
  league_name = c("Abyss", "Bestiary", "Incursion", "Delve",
                  "Betrayal", "Synthesis", "Legion", "Blight",
                  "Metamorph", "Delirium", "Harvest", "Heist"),
  start_date  = as.Date(c("2017-12-08", "2018-03-02", "2018-06-01", "2018-08-31",
                          "2018-12-07", "2019-03-08", "2019-06-07", "2019-09-06",
                          "2019-12-13", "2020-03-13", "2020-06-19", "2020-09-18"))
)

print(league_dates)

# 각 날짜에 대한 리그 메타데이터 생성
date_meta <- data.frame(date = date_vec) %>%
  mutate(
    weekday      = lubridate::wday(date, label = TRUE, week_start = 1),
    is_weekend   = lubridate::wday(date, week_start = 1) %in% c(6, 7),
    
    # 가장 가까운 (이전) 리그 시작일
    league_start = sapply(date, function(d) {
      past_starts <- league_dates$start_date[league_dates$start_date <= d]
      if (length(past_starts) == 0) return(NA)
      max(past_starts)
    }),
    
    days_since_league = as.numeric(date - as.Date(league_start)),
    
    # 리그 출시일 당일 / 첫 주 / 첫 달 dummy
    is_league_day    = (days_since_league == 0),
    is_league_week   = (days_since_league >= 0 & days_since_league <= 6),
    is_league_month  = (days_since_league >= 0 & days_since_league <= 29)
  )

# Score와 결합
scores_full <- scores_df %>%
  left_join(date_meta, by = "date")

glimpse(scores_full)
table(scores_full$is_league_day)

## 28단계: 리그 효과 검증 — FPC1, FPC2 vs 메타데이터 ----
library(ggplot2)

# FPC1 vs days_since_league
ggplot(scores_full, aes(x = days_since_league, y = FPC1)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "loess", se = TRUE, color = "darkred") +
  labs(title = "FPC1 vs days since league start",
       subtitle = "리그 사이클 내 동접 수준의 변화",
       x = "Days since league start", y = "FPC1 score") +
  theme_minimal()

# FPC2 vs days_since_league
ggplot(scores_full, aes(x = days_since_league, y = FPC2)) +
  geom_point(alpha = 0.3, color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, color = "steelblue") +
  labs(title = "FPC2 vs days since league start",
       subtitle = "리그 출시일에 일중 패턴이 비정상",
       x = "Days since league start", y = "FPC2 score") +
  theme_minimal()

# Boxplot: 리그 시작일 vs 평소
scores_full <- scores_full %>%
  mutate(period = case_when(
    is_league_day        ~ "League day (0)",
    days_since_league <= 6  ~ "Week 1 (1-6)",
    days_since_league <= 29 ~ "Month 1 (7-29)",
    days_since_league <= 60 ~ "Month 2 (30-59)",
    TRUE                    ~ "Late (60+)"
  ),
  period = factor(period, levels = c("League day (0)", "Week 1 (1-6)",
                                     "Month 1 (7-29)", "Month 2 (30-59)",
                                     "Late (60+)")))

ggplot(scores_full, aes(x = period, y = FPC1, fill = period)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "FPC1 by period within league cycle",
       x = NULL, y = "FPC1 score") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))

# 주말 효과
ggplot(scores_full, aes(x = is_weekend, y = FPC1, fill = is_weekend)) +
  geom_boxplot() +
  labs(title = "Weekend effect on FPC1",
       x = "Weekend?", y = "FPC1 score") +
  theme_minimal() +
  theme(legend.position = "none")

## 29단계: Function-on-Scalar Regression (FoSR) ----
library(fda)

# 1. 회귀에 쓸 covariate 행렬 만들기
#    X 행: 날짜, X 열: covariate
X_design <- with(scores_full, 
                 cbind(
                   intercept     = 1,
                   weekend       = as.numeric(is_weekend),
                   league_day    = as.numeric(is_league_day),
                   league_week1  = as.numeric(is_league_week & !is_league_day),
                   league_month1 = as.numeric(is_league_month & !is_league_week)
                 )
)

dim(X_design)
head(X_design)
colSums(X_design)

# 2. 회귀계수에 사용할 basis (β(t)들도 함수)
#    - β들은 평활화 필요
beta_basis <- create.bspline.basis(
  rangeval = c(0, 24),
  nbasis   = 21,        # 베타 함수는 더 적은 basis로
  norder   = 4
)

# 각 covariate에 대한 fdPar (penalty)
beta_fdPar <- fdPar(beta_basis, Lfdobj = 2, lambda = 1)
beta_list  <- list(
  intercept     = beta_fdPar,
  weekend       = beta_fdPar,
  league_day    = beta_fdPar,
  league_week1  = beta_fdPar,
  league_month1 = beta_fdPar
)

# 3. fRegress 실행
xfdlist <- list(
  intercept     = rep(1, nrow(X_design)),
  weekend       = X_design[, "weekend"],
  league_day    = X_design[, "league_day"],
  league_week1  = X_design[, "league_week1"],
  league_month1 = X_design[, "league_month1"]
)

fregress_result <- fRegress(
  y       = poe_fd,
  xfdlist = xfdlist,
  betalist = beta_list
)

# 결과 확인
str(fregress_result, max.level = 1)

## 30단계: 회귀 계수 함수 시각화 ----
# 각 효과 함수를 grid에서 평가
beta_eval <- sapply(fregress_result$betaestlist, function(b) {
  eval.fd(t_grid, b$fd)
})

# 데이터프레임으로 정리
beta_df <- data.frame(
  hour          = t_grid,
  intercept     = beta_eval[, "intercept"],
  weekend       = beta_eval[, "weekend"],
  league_day    = beta_eval[, "league_day"],
  league_week1  = beta_eval[, "league_week1"],
  league_month1 = beta_eval[, "league_month1"]
)

# Intercept (평균 곡선)
p_intercept <- ggplot(beta_df, aes(x = hour, y = intercept)) +
  geom_line(color = "black", linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  labs(title = "Baseline daily curve (intercept)",
       x = "Hour (UTC)", y = "log(1+players)") +
  theme_minimal()

# 효과 함수들 (long format)
library(tidyr)
effect_df <- beta_df %>%
  select(-intercept) %>%
  pivot_longer(-hour, names_to = "effect", values_to = "value") %>%
  mutate(effect = factor(effect, 
                         levels = c("weekend", "league_day", 
                                    "league_week1", "league_month1"),
                         labels = c("Weekend", "League day", 
                                    "League week 1", "League month 1")))

p_effects <- ggplot(effect_df, aes(x = hour, y = value, color = effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ effect, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Time-varying effects on intraday curve",
       x = "Hour (UTC)", y = "Effect on log(1+players)") +
  theme_minimal() +
  theme(legend.position = "none")

print(p_intercept)
print(p_effects)

## 31단계: 신뢰구간 / 유의성 평가 ----
## ---------- 31단계: 신뢰구간 / 유의성 평가 ----------

# 1. 잔차 계산
y_hat_eval <- eval.fd(t_grid, fregress_result$yhatfdobj)
y_obs_eval <- eval.fd(t_grid, poe_fd)
residuals  <- y_obs_eval - y_hat_eval

# 2. 시간대별 잔차 분산
SigmaE <- diag(apply(residuals, 1, var))
cat("Residual variance: range =", round(range(diag(SigmaE)), 4), "\n")

# 3. y2cMap (관측 → basis 계수 변환 행렬) 다시 계산
y2cMap <- smooth.basis(t_grid, X_t, fdPar_best)$y2cMap
cat("y2cMap dimensions:", dim(y2cMap), "\n")

# 4. 표준오차 계산
fregress_se <- fRegress.stderr(fregress_result, y2cMap, SigmaE)

# 구조 확인
cat("\n=== fregress_se 구조 ===\n")
cat("Length of betastderrlist:", length(fregress_se$betastderrlist), "\n")
cat("Names:", names(fregress_se$betastderrlist), "\n")
cat("Class of [[1]]:", class(fregress_se$betastderrlist[[1]]), "\n")

# 5. 각 효과의 SE를 grid에서 평가
se_list <- list()
effect_names <- c("intercept", "weekend", "league_day", "league_week1", "league_month1")

for (i in seq_along(fregress_se$betastderrlist)) {
  se_obj <- fregress_se$betastderrlist[[i]]
  
  # fd object인 경우와 list인 경우 모두 대응
  if (inherits(se_obj, "fd")) {
    se_vals <- as.numeric(eval.fd(t_grid, se_obj))
  } else if (is.list(se_obj) && !is.null(se_obj$fd)) {
    se_vals <- as.numeric(eval.fd(t_grid, se_obj$fd))
  } else {
    cat("Warning: SE element", i, "has unknown structure\n")
    next
  }
  
  se_list[[ effect_names[i] ]] <- se_vals
  cat("SE[", effect_names[i], "]: length =", length(se_vals),
      ", range =", round(range(se_vals), 4), "\n")
}

# 6. 신뢰구간 데이터프레임 (intercept 제외, covariate 효과만)
library(dplyr)
library(tidyr)
library(ggplot2)

beta_ci_df <- data.frame(
  hour     = rep(t_grid, 4),
  effect   = rep(c("Weekend", "League day", "League week 1", "League month 1"),
                 each = length(t_grid)),
  estimate = c(beta_eval[, "weekend"],
               beta_eval[, "league_day"],
               beta_eval[, "league_week1"],
               beta_eval[, "league_month1"]),
  se       = c(se_list[["weekend"]],
               se_list[["league_day"]],
               se_list[["league_week1"]],
               se_list[["league_month1"]])
) %>%
  mutate(
    lower  = estimate - 1.96 * se,
    upper  = estimate + 1.96 * se,
    effect = factor(effect, levels = c("Weekend", "League day",
                                       "League week 1", "League month 1"))
  )

# 7. 신뢰구간 plot
p_ci <- ggplot(beta_ci_df, aes(x = hour, y = estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.3, fill = "steelblue") +
  geom_line(color = "darkblue", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~ effect, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(title = "Time-varying effects with 95% CI",
       subtitle = "0을 포함하지 않는 구간에서 효과가 통계적으로 유의",
       x = "Hour (UTC)", y = "Effect on log(1+players)") +
  theme_minimal()

print(p_ci)

## 32단계: 모형 적합도 평가 ----
## ---------- 32단계: 시간대별 R² ----------

# 시간대별 R²
ss_total <- apply(y_obs_eval, 1, function(x) sum((x - mean(x))^2))
ss_resid <- apply(residuals, 1, function(x) sum(x^2))
r_squared_t <- 1 - ss_resid / ss_total

r2_df <- data.frame(hour = t_grid, r_squared = r_squared_t)

# Plot
p_r2 <- ggplot(r2_df, aes(x = hour, y = r_squared)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(title = "Time-varying R²",
       subtitle = "회귀모형이 어느 시간대를 잘 설명하는가?",
       x = "Hour (UTC)", y = expression(R^2)) +
  theme_minimal()

print(p_r2)

# 요약 통계
cat("\n=== 모형 적합도 ===\n")
cat("Mean R²:    ", round(mean(r_squared_t), 4), "\n")
cat("Median R²:  ", round(median(r_squared_t), 4), "\n")
cat("R² range:   ", round(range(r_squared_t), 4), "\n")

# 가장 잘 설명되는 시간대 / 잘 안 되는 시간대
cat("\n가장 잘 설명되는 시간대:\n")
print(r2_df[which.max(r_squared_t), ])
cat("\n가장 잘 설명 안 되는 시간대:\n")
print(r2_df[which.min(r_squared_t), ])

## 33단계: 함수형 이상치 탐지 — Outliergram + MS plot ----
# 패키지 설치 (필요 시)
# install.packages("roahd")
library(roahd)

# fData 객체
poe_fdata <- fData(grid = t_grid, values = X_data_imp)

# Outliergram - 기본 호출 (xlab/ylab 인자 제거)
out_result <- outliergram(poe_fdata, display = TRUE)

# Outliergram 결과의 magnitude/shape index 활용
# (out_result에 있는 다른 정보 확인)
str(out_result, max.level = 1)
names(out_result)

# 이상치 날짜 추출
outlier_idx <- out_result$ID_outliers
cat("Outliergram detected", length(outlier_idx), "outliers\n")

if (length(outlier_idx) > 0) {
  outlier_dates <- date_vec[outlier_idx]
  cat("Outlier dates:\n")
  print(outlier_dates)
}

## 34
# install.packages("fdaoutlier")
library(fdaoutlier)
library(roahd)

# msplot에 행렬 직접 입력 (행 = 곡선, 열 = 시간점)
ms_result <- fdaoutlier::msplot(X_data_imp,
                           data_depth = "MBD",
                           n_projections = 200L,
                           seed = 5021)

str(ms_result, max.level = 1)
names(ms_result)

## 35단계: 두 방법 결과 종합 + 메타데이터 결합 ----
library(dplyr)

# Outliergram 결과
outlier_og  <- date_vec[outlier_idx]

# MS plot 결과
outlier_ms  <- date_vec[ms_result$outliers]

# 교집합 (가장 확실한 outlier)
outlier_strong <- intersect(outlier_og, outlier_ms)

# 합집합 (전체 outlier 후보)
outlier_all <- union(outlier_og, outlier_ms)

cat("Outliergram only:    ", length(setdiff(outlier_og, outlier_ms)), "\n")
cat("MS plot only:        ", length(setdiff(outlier_ms, outlier_og)), "\n")
cat("Both (강력한 outlier):", length(outlier_strong), "\n")
cat("Total unique:        ", length(outlier_all), "\n")

# 메타데이터와 결합
outlier_summary <- scores_full %>%
  filter(date %in% outlier_all) %>%
  mutate(
    by_outliergram = date %in% outlier_og,
    by_msplot      = date %in% outlier_ms,
    by_both        = date %in% outlier_strong,
    
    # 분류
    type = case_when(
      is_league_day                    ~ "League start day",
      is_league_week & !is_league_day  ~ "League week 1",
      FPC1 < -3                        ~ "League fatigue (low)",
      FPC1 > 3 & !is_league_week       ~ "Mid-league spike (event)",
      FPC2 > 1                         ~ "Shape: event-like",
      FPC2 < -1                        ~ "Shape: maintenance",
      TRUE                             ~ "Other"
    )
  )

# 유형별 빈도
cat("\n=== Outlier type distribution ===\n")
type_count <- table(outlier_summary$type, outlier_summary$by_both)
print(type_count)

# 가장 확실한 outlier (양쪽 알고리즘에 잡힘) 보기
cat("\n=== Strong outliers (둘 다에서 잡힘) ===\n")
print(outlier_summary %>% 
        filter(by_both) %>% 
        arrange(date) %>% 
        select(date, weekday, type, FPC1, FPC2, days_since_league))

## 36단계: 유형별 outlier 곡선 시각화 ----
library(ggplot2)

# poe long format에 분류 결합
outlier_type_map <- outlier_summary %>% 
  select(date, type) %>% 
  mutate(date = as.character(date))

poe_long <- poe %>%
  filter(!is.na(log_players)) %>%
  mutate(
    date_str   = as.character(date_utc),
    is_outlier = date_str %in% outlier_type_map$date
  ) %>%
  left_join(outlier_type_map, by = c("date_str" = "date")) %>%
  mutate(plot_type = ifelse(is_outlier, type, "Normal"))

# 유형별 facet plot
ggplot() +
  geom_line(data = poe_long %>% filter(!is_outlier),
            aes(x = hour_decimal, y = log_players, group = date_utc),
            color = "gray85", alpha = 0.05) +
  geom_line(data = poe_long %>% filter(is_outlier),
            aes(x = hour_decimal, y = log_players, group = date_utc, color = type),
            alpha = 0.5, linewidth = 0.5) +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~ plot_type, ncol = 3) +
  labs(title = "Outlier curves by type (UTC)",
       subtitle = paste0(length(outlier_all), " outlier days, ", 
                         length(outlier_strong), " detected by both methods"),
       x = "Hour (UTC)", y = "log(1+players)",
       color = "Type") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 9))

## 37단계: 케이스 스터디 — 4개 인상적인 outlier ----
# 흥미로운 케이스 4개
case_dates <- as.Date(c(
  "2020-03-13",  # Delirium 출시 + 코로나 락다운
  "2018-12-09",  # Betrayal 출시 다음날 (역대 최고)
  "2018-12-03",  # League fatigue 최저 (Delve 마지막 주)
  "2019-10-09"   # 점검일 (Shape: maintenance)
))

case_labels <- c(
  "2020-03-13: Delirium release + COVID lockdown",
  "2018-12-09: Betrayal day 2 (peak)",
  "2018-12-03: Pre-league fatigue (lowest)",
  "2019-10-09: Maintenance day"
)

case_data <- poe %>%
  filter(date_utc %in% case_dates, !is.na(log_players)) %>%
  mutate(case_label = factor(case_labels[match(date_utc, case_dates)],
                             levels = case_labels))

# 평균 곡선
mean_eval <- as.numeric(eval.fd(t_grid, mean_fd))
mean_ref_df <- data.frame(hour_decimal = t_grid, log_players = mean_eval)

ggplot() +
  geom_line(data = case_data,
            aes(x = hour_decimal, y = log_players, color = case_label),
            linewidth = 1.2) +
  geom_line(data = mean_ref_df,
            aes(x = hour_decimal, y = log_players),
            color = "gray40", linetype = "dashed", linewidth = 1) +
  facet_wrap(~ case_label, ncol = 2) +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Case study: 4 representative outlier days",
       subtitle = "Dashed line = overall mean curve (UTC)",
       x = "Hour (UTC)", y = "log(1+players)",
       color = "Date") +
  theme_minimal() +
  theme(legend.position = "none")
