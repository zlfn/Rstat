# :yellow_heart: Rstat 

- Rstat 패키지가 3.5에서 만들어져서 4 이상 버전에서는 실행이 안 됨. <br>
- 굉장히 통계 교육에 신경을 써서 만든 패키지 같은데 이대로 방치 되버리는 상황이 아까움. :+1:
- 구형 레거시 버전에, 엘레강스하지 못하게 설치 하는 상황이 안타까워 R 4.0 이상에서 돌아가는 Rstat 패키지를 작성함.

<img src ='https://user-images.githubusercontent.com/6457691/81569878-d0980a00-93da-11ea-907f-0a8049a1b990.jpg' width = '400'>

## :wrench: 설치 방법 

아래 코드를 Rstudio 콘솔창에 그대로 입력

```R
install.packages('devtools')
library(devtools)
install_github('jhk0530/Rstat')
library(Rstat)
```

## :star: 설치 확인

```R
strat.hist()
```
를 실행 했을때 우측 상단 쯤에 다음과 같은 그림이 나오면 OK.

<img src='https://user-images.githubusercontent.com/6457691/81809159-dd903700-955b-11ea-980f-9645358a2fe6.png'>


## :blush: Authors
* :octocat: T.J. Lim (tjlim@ssu.ac.kr)
* :octocat: Jinhwan Kim [@jhk0530](http://github.com/jhk0530) 

## :memo: License
This project is [MIT](https://opensource.org/licenses/MIT) licensed

*This README was generated with :two_hearts: by [shinyReadme](http://github.com/jhk0530/shinyReadme)*

## Rstat 패키지 혹은 기타 R 관련 문의 
:email: hwanistic@gmail.com
