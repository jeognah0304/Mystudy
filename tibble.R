

  library(dplyr)
  install.packages("nycflights13")
  library(nycflights13) 
  
  # 데이터를 살펴보자
  data = nycflights13::flights
  str(nycflights13::flights)
  
  #print 행의개수 10개, 모든 열 출력
  data %>% print(n=10, width=Inf)
  
  #print: 행의개수 3개, 너비 70개=7개의 변수, 너비80개 =8개의 변수 출력.
  data %>% print(n=3, width=70)
  
  
  # 조건옵션문 options()
  # 만약 데이터가 10개를 넘어가면 5개만 출력하고, 나머지는 ... 뒤에 출력하려고 해보면
  options('tibble.print_max'=10, 'tibble.print_min'=5)
  data %>% print()  
  
  # 옵션문은 말 그대로 옵션이기 때문에, 이 자체로 어떤 출력이 발생하지 않는다. 그러나 이 옵션을 저장하지 않고도 바로 print()를 실행하면
  # 자동으로 옵션이 적용되는 것을 확인할 수 있다.
  # 여기서 데이터의 갯수는 10개를 넘어간다. 그래서 5개만 출력해서 보고 싶다면 옵션을 주면 된다.
  
  options('tibble.print_max'=10, tibble.print_min = 1)
  data %>% print()  
  # 10개가 넘는 데이터가 있으니, 1개의 데이터만 출력해라.
  
  
  # <항상 모든 행을 출력할 때> : 대신 로딩시간이 너무 길 수 있다.
  options(tibble.print_min = Inf)
  data %>% print()  
  
  
  # <출력 폭 설정>
  # 원래 기본값은 NULL이지만, 출력폭을 정할 수 있음.
  options(tibble.width=30)
  data %>% print()
  
  options(tibble.width=100)
  data %>% print()
  
  # 출력폭이 30이면 dep_time변수까지 출력이되고, 100으로 지정하면 carrier 변수까지 출력됨
  # 데이터에 따라서 원하는 정도를 설정할 수 있으니 발표나 다른사람들에게 보여줄 때 유용하게 쓸 수 있다.
  
  
  # <너비에 상관없이 모두 출력>
  options(tibble.width=Inf)
  data %>% print()
  
  # <추가 열의 이름 인쇄 개수 결정>
  options(tibble.max_extra_cols=2, tibble.width=NULL)
  data %>% print()  
  
  
  # tibble의 class확인
  class(flights)
  as_tibble(flights) %>% class()  
  
  
  
  # tibble은 간단한 data.frame이라고 보면 된다.
  #tbl_df = 티블 데이터 프레임.