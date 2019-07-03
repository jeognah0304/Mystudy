

  library(dplyr)
  install.packages("nycflights13")
  library(nycflights13) 
  
  # �����͸� ���캸��
  data = nycflights13::flights
  str(nycflights13::flights)
  
  #print ���ǰ��� 10��, ��� �� ���
  data %>% print(n=10, width=Inf)
  
  #print: ���ǰ��� 3��, �ʺ� 70��=7���� ����, �ʺ�80�� =8���� ���� ���.
  data %>% print(n=3, width=70)
  
  
  # ���ǿɼǹ� options()
  # ���� �����Ͱ� 10���� �Ѿ�� 5���� ����ϰ�, �������� ... �ڿ� ����Ϸ��� �غ���
  options('tibble.print_max'=10, 'tibble.print_min'=5)
  data %>% print()  
  
  # �ɼǹ��� �� �״�� �ɼ��̱� ������, �� ��ü�� � ����� �߻����� �ʴ´�. �׷��� �� �ɼ��� �������� �ʰ��� �ٷ� print()�� �����ϸ�
  # �ڵ����� �ɼ��� ����Ǵ� ���� Ȯ���� �� �ִ�.
  # ���⼭ �������� ������ 10���� �Ѿ��. �׷��� 5���� ����ؼ� ���� �ʹٸ� �ɼ��� �ָ� �ȴ�.
  
  options('tibble.print_max'=10, tibble.print_min = 1)
  data %>% print()  
  # 10���� �Ѵ� �����Ͱ� ������, 1���� �����͸� ����ض�.
  
  
  # <�׻� ��� ���� ����� ��> : ��� �ε��ð��� �ʹ� �� �� �ִ�.
  options(tibble.print_min = Inf)
  data %>% print()  
  
  
  # <��� �� ����>
  # ���� �⺻���� NULL������, ������� ���� �� ����.
  options(tibble.width=30)
  data %>% print()
  
  options(tibble.width=100)
  data %>% print()
  
  # ������� 30�̸� dep_time�������� ����̵ǰ�, 100���� �����ϸ� carrier �������� ��µ�
  # �����Ϳ� ���� ���ϴ� ������ ������ �� ������ ��ǥ�� �ٸ�����鿡�� ������ �� �����ϰ� �� �� �ִ�.
  
  
  # <�ʺ� ������� ��� ���>
  options(tibble.width=Inf)
  data %>% print()
  
  # <�߰� ���� �̸� �μ� ���� ����>
  options(tibble.max_extra_cols=2, tibble.width=NULL)
  data %>% print()  
  
  
  # tibble�� classȮ��
  class(flights)
  as_tibble(flights) %>% class()  
  
  
  
  # tibble�� ������ data.frame�̶�� ���� �ȴ�.
  #tbl_df = Ƽ�� ������ ������.