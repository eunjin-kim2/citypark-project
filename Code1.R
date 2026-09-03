setwd("c:/R/project")
getwd()

park <- read.csv("전국도시공원정보표준데이터.csv", header=T, fileEncoding = "EUC-KR")

str(park)
head(park)

colSums(park == "")

# 시설 컬럼 처리
park$공원보유시설.운동시설.[park$공원보유시설.운동시설. == ""] <- "없음"
park$공원보유시설.유희시설.[park$공원보유시설.유희시설. == ""] <- "없음"
park$공원보유시설.편익시설.[park$공원보유시설.편익시설. == ""] <- "없음"
park$공원보유시설.교양시설.[park$공원보유시설.교양시설. == ""] <- "없음"
park$공원보유시설.기타시설.[park$공원보유시설.기타시설. == ""] <- "없음"

# 오타 수정
park$소재지도로명주소 <- gsub("경개도", "경기도",
                      park$소재지도로명주소)

park$소재지도로명주소 <- gsub("인천광역시연수구",
                      "인천광역시 연수구",
                      park$소재지도로명주소)

# 주소 처리(소재지지번주소로 대체)
park$소재지도로명주소[park$소재지도로명주소 == ""] <- NA
park$소재지지번주소[park$소재지지번주소 == ""] <- NA

park$소재지도로명주소[is.na(park$소재지도로명주소)] <-
  park$소재지지번주소[is.na(park$소재지도로명주소)]

# 주소 처리(소재지도로명주소로로 대체)
park$소재지지번주소[is.na(park$소재지지번주소)] <-
  park$소재지도로명주소[is.na(park$소재지지번주소)]


# 좌표 없는 데이터 제거
park <- park[!is.na(park$위도) & !is.na(park$경도), ]


# 전화번호 처리
park$전화번호[park$전화번호 == ""] <- "미제공"

# 지정고시일 처리
park$지정고시일[park$지정고시일 == ""] <- NA
park$지정고시일 <- as.Date(park$지정고시일)

park$공원면적 <- as.numeric(park$공원면적)

# 확인
colSums(park == "")


str(park)
head(park)
tail(park)
unique(park$공원명)
unique(park$소재지지번주소)
range(park$지정고시일)

library(ggplot2)


park$시도 <- sapply(strsplit(park$소재지도로명주소, " "), `[`, 1)
sort(unique(park$시도))

# (1) 시도별 공원 개수########################################
count <- aggregate(park[,'공원명'], list(park$시도), length)
count
colnames(count) <- c("시도", "공원개수")

count <- count[order(count$공원개수, decreasing = TRUE), ]
count <- count[1:7, ]

count$시도 <- factor(count$시도,levels = rev(count$시도))

ggplot(count, aes(x=시도, y=공원개수, fill=공원개수)) +
  geom_bar(stat='identity',width=0.7) +
  scale_fill_gradient(low="#B7E4D1", high="#2D6A4F")+
  coord_flip() +
  labs(title='공원 가장 많은 시도 Top 7',
       x='시도',
       y='공원 수') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust=0.5, face="bold", size=16),
    axis.title = element_text(face="bold", size=12),
    axis.text = element_text(size=10),
    axis.line = element_line(color = "grey70", linewidth = 0.4),
    panel.grid.major.y = element_line(color = "grey85"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none")


#(1) 시도별 공원면적 합계########################################
# 공원면적 숫자형 변환
park$공원면적 <- as.numeric(park$공원면적)

# 시도별 합계
area <- aggregate(park$공원면적, list(park$시도), sum)

# 컬럼명 변경
colnames(area) <- c("시도", "면적")

# 큰 순서로 정렬
area <- area[order(area$면적, decreasing = TRUE), ]

# 전체 면적 단위 변환 (㎡ -> 만㎡)
area$면적 <- area$면적 / 10000

area$시도 <- factor(area$시도, levels = rev(area$시도))

ggplot(area, aes(x=시도, y=면적)) +
  geom_bar(stat='identity',
           width=0.7,
           col='steelblue',
           fill='steelblue') +
  labs(title='시도별 공원면적',
       x='시도',
       y='면적(만㎡)') +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle=45, hjust=1))

# 상위 10개 그래프
area_10 <- area[1:10, ]

ggplot(area_10, aes(x=시도, y=면적, fill=면적)) +
  geom_bar(stat='identity',width=0.7) +
  labs(title='공원 면적 넓은 시도 Top 10',
       x='시도',
       y='면적(만㎡)') +
  scale_fill_gradient(low="#B7E4D1", high="#2D6A4F")+
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, face="bold", size=16),
        axis.title = element_text(face="bold", size=12),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.line = element_line(color = "grey70", linewidth = 0.4),
        panel.grid.major.y = element_line(color = "grey85"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")+
  coord_flip()

# 공원면적 TOP10
park_top10 <- park[order(park$공원면적, decreasing = TRUE), ][1:10, ]




# (2) 시도별 평균 공원면적########################################
avg <- aggregate(park$공원면적, list(park$시도), mean)
colnames(avg) <- c("시도", "평균면적")

avg <- avg[order(avg$평균면적, decreasing = TRUE), ]

avg$평균면적 <- avg$평균면적 / 10000

avg$시도 <- factor(avg$시도, levels = rev(avg$시도))

avg_7 <- avg[1:7, ]

ggplot(avg_7, aes(x=시도, y=평균면적, fill=평균면적)) +
  geom_bar(stat='identity', width=0.7) +
  scale_fill_gradient(low="#B7E4D1", high="#2D6A4F") +
  coord_flip() +
  labs(title='대규모 공원 많은 시도 Top 7 (평균 면적 기준)',
       x='시도',
       y='평균면적(만㎡)') +
  theme_minimal() +
  theme(
    plot.title = element_text(vjust=0.4, face="bold", size=16),
    axis.title = element_text(face="bold", size=12),
    axis.text = element_text(size=10),
    axis.line = element_line(color="grey70", linewidth=0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color="grey85"),
    panel.grid.minor = element_blank(),
    legend.position = "none")+
  geom_text(aes(label = round(평균면적, 1)),hjust = -0.3,size = 3.5)





# (3) 시도별 공원 면적 분포 (경기도, 인청광역시, 서울특별시, 경상남도, 충청남도)
# 면적 상위5
# 시도별 공원면적 합계 계산
area_5 <- aggregate(park$공원면적, list(park$시도), sum)
colnames(area_5) <- c("시도", "공원면적")
area_5 <- area_5[order(area_5$공원면적, decreasing=TRUE), ][1:5, ]

# 상위 5개 시도의 원본 공원 데이터 추출
park_5 <- park[park$시도 %in% area_5$시도, ]

park_5$시도 <- factor(park_5$시도,levels = area_5$시도)

# 상자그림 생성 (㎡ → 만㎡ 변환)
park[order(park$공원면적, decreasing=TRUE),
     c("공원명","시도","공원면적")][1:10,]

# 상자그림
ggplot(park_5, aes(x = 시도, y = 공원면적 / 10000)) +
  geom_boxplot(
    fill = "#B7E4D1",
    outlier.color = "#52B788",
    outlier.size = 2
  ) +
  labs(
    title = "공원 면적 TOP 5 시도 분포",
    x = "시도",
    y = "면적(만㎡)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust=0.5, face="bold", size=16),
    axis.title = element_text(face="bold", size=12),
    axis.text = element_text(size=10),
    axis.line = element_line(color="grey70", linewidth=0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color="grey85"),
    panel.grid.minor = element_blank(),
    legend.position = "none")






# (4) 공원구분별 공원 수########################################
count2 <- aggregate(park[,'공원명'], list(park$공원구분), length)

colnames(count2) <- c("공원구분", "공원개수")

count2 <- subset(count2, 공원구분 != "기타")
count2 <- count2[order(count2$공원개수, decreasing = TRUE), ]
count2 <- count2[1:7, ]

# ggplot 순서 적용
count2$공원구분 <- factor(count2$공원구분,
                      levels = count2$공원구분)

ggplot(count2, aes(x=공원구분, y=공원개수, fill=공원개수)) +
  geom_bar(stat='identity', width=0.7) +
  geom_text(aes(label=공원개수),
            vjust=-0.3,
            size=3.5) +
  scale_fill_gradient(low="#B7E4D1", high="#2D6A4F") +
  labs(title='공원유형별 분포',
       x='공원유형',
       y='공원 수') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust=0.5, face="bold", size=16),
    axis.title = element_text(face="bold", size=12),
    axis.text = element_text(size=10),
    axis.line = element_line(color="grey70", linewidth=0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color="grey85"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )




# (5) 연도별 누적 공원 조성 수########################################
park$연도 <- format(park$지정고시일, "%Y")
park.year <- aggregate(park[,'공원명'],by=list(연도=park$연도), length)
colnames(park.year)[2] <- "공원조성수"
park.year <- park.year[order(park.year$연도), ]

# 누적 공원 수 계산 cumsum
park.year <- subset(park.year, 연도 >= 2000 & 연도 <= 2025)
park.year$누적공원수 <- cumsum(park.year$공원조성수)

ggplot(park.year, aes(x=연도, y=누적공원수, group=1)) +
  geom_line(color="#2D6A4F", size=2) +
  geom_point(size=2, shape=21, fill="white", color="black", stroke=0.8) +
  labs(title='연도별 전국 도시공원 누적 조성 수',
       x='연도',
       y='공원조성수') +
  scale_x_discrete(breaks = seq(2000, 2025, 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust=0.5, face="bold", size=16),
    axis.title = element_text(face="bold", size=12),
    axis.text = element_text(size=10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "grey70", linewidth = 0.4),
    panel.grid.major.y = element_line(color = "grey85"))






# (6) 지도
library(dplyr)
library(leaflet)
library(htmltools)

setwd("c:/R/project")
getwd()

park <- read.csv("전국도시공원정보표준데이터.csv", header=T, fileEncoding = "EUC-KR")
head(park)
str(park)

# 2. 팝업 내용 작성
popup_text <- paste0("<b>", park$공원명, "</b><br>", 
                     park$소재지도로명주소,"<br>", 
                     park$공원면적,"㎡")
popup_html <- lapply(popup_text, HTML)

# 3. 지도 생성
m_park <- leaflet(park) %>%
  # 지도 배경
  addProviderTiles(providers$CartoDB.Positron) %>%
  # 전국 중앙 좌표
  setView(
    lat = 36.5,
    lng = 127.8,
    zoom = 7 ) %>%
  # 마커 추가
  addCircleMarkers(
    lat = ~위도,
    lng = ~경도,
    radius = 5,
    color = "#2D6A4F",
    fillColor = "#74C69D",
    fillOpacity = 0.8,
    popup = popup_html,
    clusterOptions = markerClusterOptions()
  )
  
m_park



library(treemap)

facility <- data.frame(
  시설 = c("운동시설", "유희시설", "편익시설", "교양시설", "기타시설"),
  개수 = c(
    sum(park$공원보유시설.운동시설. != ""),
    sum(park$공원보유시설.유희시설. != ""),
    sum(park$공원보유시설.편익시설. != ""),
    sum(park$공원보유시설.교양시설. != ""),
    sum(park$공원보유시설.기타시설. != "")
  )
)

treemap(
  facility,
  index = "시설",
  vSize = "개수",
  title = "도시공원 보유시설 분포"
)
