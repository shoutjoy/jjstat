# AI 개발 검증

패키지 루트에서 `Rscript --vanilla tools/ai/verify_exaone.R` 등의 검증 스크립트를 실행합니다. Windows R 시작 로캘이 UTF-8을 지원하지 않으면 `Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')` 후 source합니다.

검증은 로컬 모의 서버를 사용합니다. 실제 키나 실제 공급자를 호출하지 않습니다. Node.js는 모의 서버와 JavaScript 검증에, Playwright·Edge는 verify_web.R의 브라우저 검증에만 필요합니다. PLAYWRIGHT_MODULE로 기존 설치 위치를 지정할 수 있습니다.

- verify_public_api, verify_exaone, verify_providers: 공개 함수와 공급자 계약
- verify_metrics, verify_progress, verify_streaming, verify_stop: 지표·스트림·중지
- verify_math.R 다음 verify_math.js: 수식과 번들 자산
- verify_bold, verify_console, verify_windows: 서식·콘솔 전송·Tk 질문창
- verify_web: 실제 브라우저에서 복사·다운로드·질문 기록·중지

수식·브라우저 결과는 verification/에 생성되며 빌드와 Git에서 제외합니다. tools/ai/ui-build는 UI 번들 재생성 소스입니다. 해당 폴더에서 npm ci 후 node build.cjs로 vendor.js를 재생성할 수 있습니다. THIRD_PARTY_LICENSES.txt를 함께 유지해야 합니다.
