RUN := +Bc +K true -smp enable -pa ebin -s crypto -s inets -s ssl

all:
	rebar get-deps && rebar compile

clean:
	rebar clean

build_plt: all
	rebar skip_deps=true build-plt

analyze: all
	rebar dialyze

update-deps:
	rebar update-deps

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref

debug: all
	erl  -boot start_sasl ${RUN} -s apns -s appmon -s debugger

run: all
	if [ -f `hostname`.config ]; then\
		erl  -config `hostname` -boot start_sasl ${RUN} -s apns;\
	else\
		erl  -boot start_sasl ${RUN} -s apns;\
	fi

shell: all
	if [ -f `hostname`.config ]; then\
		erl  -config `hostname` -boot start_sasl ${RUN};\
	else\
		erl  -boot start_sasl ${RUN};\
	fi

test: all
	if [ -f `hostname`.config ]; then\
		erl -noshell -noinput -config `hostname` ${RUN} -run apns_tests main;\
	else\
		erl -noshell -noinput ${RUN} -run apns_tests main;\
	fi

test_mng_cases: all
	if [ -f `hostname`.config ]; then\
		erl -noshell -noinput -config `hostname` ${RUN} -run apns_manager_tests main;\
	else\
		erl -noshell -noinput ${RUN} -run apns_manager_tests main;\
	fi

test_tcp: all
	if [ -f `hostname`.config ]; then\
		erl -noshell -noinput -config `hostname` ${RUN} -run tcp_server_tests main;\
	else\
		erl -noshell -noinput ${RUN} -run tcp_server_tests main;\
	fi

run_mng: all
	if [ -f `hostname`.config ]; then \
		erl -noinput -config `hostname` ${RUN} -run apns_manager_app start; \
	else \
		erl -noinput ${RUN} -run apns_manager_app start; \
	fi

NC := nc localhost 2222
DATE := `date +"%m%d-%H:%M:%S"`
CORRECT_DEVICE_TOKEN := 130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d49
ERROR_DEVICE_TOKEN := 130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d48
test_mng: all test_mng_cases
	erl  -noshell -boot start_sasl ${RUN}  -s appmon -s apns -sname test_mng &
	sleep 5
	#echo "apnsm PushTestDev3 ${ERROR_DEVICE_TOKEN} ${DATE}-hello3 3 chime 86400 {\"key\":3}" | ${NC}
	#echo "apnsm PushTestDev4 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello41 41 chime 86400 {\"key\":41}" | ${NC}
	#echo "apnsm PushTestDev3 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello32 32 chime 86400 {\"key\":32}" | ${NC}
	#echo "apnsm PushTestDev4 ${ERROR_DEVICE_TOKEN} ${DATE}-hello4 4 chime 86400 {\"key\":4}" | ${NC}
	#echo "apnsm PushTestDev4 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello42 42 chime 86400 {\"key\":42}" | ${NC}
	#echo "apnsm PushTestDev4 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello43 43 chime 86400 {\"key\":43}" | ${NC}
	#echo "apnsm PushTestDev4 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello44 44 chime 86400 {\"key\":44}" | ${NC}
	./test_data.sh
	sleep 60
	kill `pgrep -f "beam.*-sname test_mng"`

.PHONY: test_mng run
