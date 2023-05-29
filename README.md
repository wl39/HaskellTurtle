# HaskellTurtle
- 사용 언어: Haskell
- Haskell을 기반으로 만든, Turtle++ 언어 생성
	- Turtle++은 (https://turtleacademy.com/)에서 영감을 받아 만든 프로그램입니다. Turtle++에는 총 6개의 built-in 함수가 존재합니다.
		- penDown: Turtle이 펜을 내려, 화면에 그림을 그릴준비를 합니다. (Turtle의 pen이 down인 상태에서 fd 또는 moveTo 함수를 통해 이동하게 되면, 화면에는 Turtle의 펜 색상에 맞는 선분을 그리게 됩니다.)
		- penUp: Turtle이 펜을 올려, 더 이상 움직여도 그림을 그릴 수 없게 됩니다.
		- changeCol (Color): Turtle이 펜의 색상을 (Color)로 변경합니다. Turtle이 선택가능한 색상은 Haskell Colour package에 존재하는 색상만 가능합니다.
		- rt (degree): Turtle이 현재 위치에서 시계방향으로 degree만큼 회전합니다. 
		- lt (degree): Turtle이 현재 위치에서 반시계방향으로 degree만큼 회전합니다. 
		- fd (number): Turtle이 (number) 만큼 앞으로 이동합니다.
		- moveTo (coordination): Turtle의 현재 좌표를 (coordination)으로 옮깁니다. 이 과정중에서 Turtle은 그림을 그릴 수 없습니다. 
- 개발기간: 21일
- 마지막 수정일: 2019년 4월
  
## 사용방법
- **Haskell과 cabal이 컴퓨터내에 설치되어 있어야 합니다.**
- Extensions폴더 안에서 아래의 명령어를 실행하면 됩니다.
	- `cabal v2-run Turtle "../../Example-Programs/<.tpp file>" `
- 프로그램이 실행 되면 유저는 2개의 매개변수를 터미널에서 입력해야합니다.
	1. 비교 작업에서 모든 부동 소수점을 반올림하려는 소수 자릿수
	2. Frame rate
- 위의 모든 과정을 완료하게 되면, Turtle은 유저가 입력한 .tpp 파일에 맞춰 그림을 그리게 됩니다.
---
## 프로그램에서 주목해서 봐야할 점
- 함수형 언어 Haskell 사용
- 재귀적으로 함수를 호출해, 여러가지 expression과 statement을 구현
