# LG EXAONE 사용 준비 · 2026-11-16

LG 공식 API를 사용하는 공급자 `exaone`를 jjstat에 통합했습니다.

## API 키만 입력하기

`library(jjstat)`로 패키지를 불러온 뒤 사용합니다.

```r
# library(jjstat) 실행 후
exaone_setting()  # 주소와 모델은 기본 입력됨. 키 입력 → 설정 저장
exaone('이 분석결과를 설명해줘')

# 코드로 키만 설정하는 방법 (현재 세션에만 보관)
exaone_setting(api_key='발급받은 키', open=FALSE)
results |> exaone('이 결과를 표와 학술 보고서로 정리해줘')
results |> exaone_window()

# RStudio 외 환경
answer <- exaone('질문', viewer=FALSE)
```

`EXAONE_API_KEY` 환경변수도 지원합니다. 실제 키를 공유 코드나 저장소에 넣지 마십시오. API 키는 설정 파일·질문 기록·브라우저 설정에 저장하지 않습니다. HTTP 통신과 별도 R 작업 프로세스 시작에는 삭제되는 임시 파일을 사용합니다.

## 기본값과 동작

- API 기본 주소: `https://api.exaone.ai/v1`
- 생성 경로: `/chat/completions`
- 기본 모델: `k-exaone_v2.5`
- Bearer 인증. 별도 서버 설치·포트 설정·모델 다운로드 불필요.
- `reasoning_effort='low'`, `temperature=1`, `max_tokens=16384`, `stream=TRUE`.
- EXAONE는 항상 추론합니다. `max_tokens`는 추론과 답변을 함께 포함합니다. 추론 이벤트는 진행 상태로 처리하며 최종 보고서 본문에는 섞지 않습니다.
- `Output`은 서버가 보고한 completion_tokens이며 추론을 포함할 수 있습니다. 별도의 reasoning_tokens도 내부 metrics에 보존합니다. 화면의 Output을 보이는 답변만의 토큰 수로 해석하지 마십시오.
- `exaone(..., reasoning_effort='high', max_tokens=32768)`처럼 조정할 수 있습니다.
- `ai_models('exaone')`는 공식 문서의 모델 목록을 반환합니다. 네트워크 연결·키 인증 성공을 의미하지 않습니다. 미확인 `/models` API는 호출하지 않습니다.
- `ai_test_connection('exaone')`는 실제 짧은 생성으로 연결을 검사합니다. 설정창의 연결 확인/모델 사용 확인 버튼도 생성 요청을 보냅니다. 로컬 모델 로딩 API는 사용하지 않습니다.
- 키가 없으면 요청 전에 설정 방법을 안내합니다. 인증 실패 메시지에서는 키를 가립니다.
- `exaone_window()`는 설정된 세션 키를 사용합니다. 설정 변경 후에는 새 창을 여십시오.
- 데이터프레임 기본 100행, 입력 텍스트 30,000자 제한은 기존 공급자와 같습니다. 필요한 경우 `max_rows`, `max_chars`를 조정합니다.

## 검증 범위

`tools/ai/verify_exaone.R`에서 공식 형식의 모의 SSE/JSON 응답을 사용해 기본값, 인증, 설정 저장 시 키 제외, 추론 분리, 토큰 지표, 이어쓰기, 중지, 오류 키 마스킹, 실제 별도 R 작업 프로세스와 기록을 검사합니다. 실제 LG API 키를 이용한 서비스 연결이나 최종 RStudio 화면 검증은 별도로 필요합니다.

## 공식 근거

- [EXAONE 빠른 시작](https://docs.exaone.ai/en/docs/quick-start)
- [K-EXAONE 2.5 요청·응답 사양](https://docs.exaone.ai/en/docs/k-exaone-2.5)
- [인증](https://docs.exaone.ai/en/docs/authentication)

API 기본값은 2026-11-16 확인한 공식 사양 기준입니다.
