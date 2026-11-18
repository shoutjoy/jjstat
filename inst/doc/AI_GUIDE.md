# jjstat AI 사용 안내

## 시작하기

```r
library(jjstat)

# LG EXAONE: 창에 API 키만 입력한 뒤 설정 저장
exaone_setting()
exaone('안녕하세요')

# 분석 결과를 파이프로 전달
results |> exaone('결과를 표로 정리하고 학술 보고서로 작성해줘')
results |> exaone_window()
```

`exaone_setting(api_key='발급받은 키', open=FALSE)`로 현재 세션의 키만 설정할 수도 있습니다. 실제 키는 공유 R 스크립트나 저장소에 넣지 마십시오. `EXAONE_API_KEY` 환경변수도 지원합니다.

## 공급자별 함수

| 공급자 | 질문 | 설정 | 독립 질문창 | 기본 주소 |
|---|---|---|---|---|
| LG EXAONE | `exaone()` | `exaone_setting()` | `exaone_window()` | https://api.exaone.ai/v1 |
| Ollama | `ollama()` | `ollama_setting()` | `ollama_window()` | http://127.0.0.1:11434 |
| LM Studio | `lmstudio()` | `lmstudio_setting()` | `lmstudio_window()` | http://127.0.0.1:5678 |
| Google AI Studio | `aistudio()` | `aistudio_setting()` | `aistudio_window()` | https://generativelanguage.googleapis.com/v1beta |
| Lemonade | `lemonade()` | `lemonade_setting()` | `lemonade_window()` | http://127.0.0.1:13305/api/v1 |

로컬 공급자는 해당 서버를 실행한 후 주소·모델을 설정합니다. LM Studio의 기본 포트는 5678이며 서버가 1234 등을 사용하면 설정창에서 바꿉니다. Google AI Studio는 API 키와 사용 가능한 모델을 설정합니다. EXAONE는 공식 주소와 `k-exaone_v2.5`가 기본값이므로 키만 입력하면 됩니다.

```r
lmstudio_setting(port=1234, model='실제 모델 ID', open=FALSE)
results |> lmstudio('분석결과를 설명해줘')
```

## 출력과 이어쓰기

공급자 질문 함수는 기본적으로 RStudio Viewer에 스트리밍합니다. RStudio 밖에서는 `viewer=FALSE`를 지정합니다. 별도 질문창은 RStudio가 없어도 사용할 수 있습니다. 창을 열 때 생성 요청을 자동 전송하지 않으며 질문 실행 버튼 또는 Ctrl+Enter로 시작합니다.

```r
answer <- exaone('질문', viewer=FALSE)
answer <- ai_last_response()
answer <- ai_continue('논의와 결론을 추가해줘', viewer=FALSE)
```

Markdown/PV 전환, 표·코드·본문 복사, Markdown 다운로드, 수식·Mermaid 렌더링, 중지와 질문 기록을 지원합니다. R 코드의 toConsole은 RStudio 콘솔에 입력만 전달하고 자동 실행하지 않습니다. Tcl/Tk는 설정창과 `engine='tk'` 질문창에서 사용합니다.

RStudio Viewer 요청은 Viewer의 중지 버튼 또는 RStudio Esc/Stop으로 중지합니다. 독립 질문창에는 자체 중지 버튼이 있습니다. `ai_stop()`은 콜백 등에서 사용할 수 있습니다. 받은 부분 응답은 보존하며 `ai_continue()`로 이어갈 수 있습니다.

응답이 토큰 제한으로 종료되면 기본 최대 3회 자동 이어쓰기를 합니다. `auto_continue=FALSE`로 끄거나 `max_continuations`로 조정할 수 있습니다. EXAONE는 `max_tokens=16384`, `reasoning_effort='low'`, `temperature=1`이 기본이며 추론과 출력이 토큰 한도를 함께 사용합니다. 나머지 공급자의 기본 출력 한도는 4096입니다.

## 데이터와 기록

데이터프레임은 기본 100행, 문자열은 기본 30,000자까지 전송합니다. 초과 부분은 생략 표시를 붙입니다. `max_rows`, `max_chars`로 조정할 수 있습니다. 클라우드 공급자(EXAONE·AI Studio)를 호출하면 전달한 분석 내용이 해당 서비스로 전송됩니다.

API 키는 설정 파일과 질문 기록에 저장하지 않습니다. 주소·모델은 선택적으로 다음 세션에 저장하며 HTTP 요청과 별도 R 작업 프로세스 시작에는 삭제되는 임시 파일을 사용합니다. 질문창 기록은 질문과 응답을 저장하므로 분석 내용이 포함될 수 있습니다.

```r
ai_settings('exaone')  # 키 원문을 제외한 설정
ai_models('exaone')    # 공식 문서 기반 모델 목록; 인증 검사 아님
ai_test_connection('exaone')  # 실제 짧은 응답 요청
ai_history()
file.path(tools::R_user_dir('jjstat','data'), 'ai-history')
```

주 R 세션의 `ai_last_response()`와 질문창의 별도 작업 프로세스 결과는 분리됩니다. 질문창의 결과는 화면과 기록에서 확인합니다. 설정을 바꾸면 질문창을 새로 여십시오.

## 실행 환경

R 패키지 의존성은 DESCRIPTION에 선언되어 있습니다. HTTP 요청에는 PATH의 `curl.exe`(Windows) 또는 `curl`이 필요합니다. Node.js와 Playwright는 개발 검증에만 필요하며 일반 사용에는 필요하지 않습니다. 렌더러·폰트·라이선스는 패키지에 포함되어 CDN 없이 작동합니다.

EXAONE 세부 내용은 [AI_EXAONE.md](AI_EXAONE.md), 함수별 도움말은 `?exaone`, `?ai_window`, `?ai_settings`를 참고하십시오. 모델의 보고서 내용과 통계적 해석은 사용자가 확인해야 합니다.
