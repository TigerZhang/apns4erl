#!/usr/bin/env bash
NC="nc localhost 2222"
DATE=`date +"%m%d-%H:%M:%S"`
CORRECT_DEVICE_TOKEN=130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d49
ERROR_DEVICE_TOKEN=130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d48

for i in {41001..41020}
do
    echo "apnsm PushTestDev4 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello${i} ${i} chime 86400 {\"key\":${i}}" | ${NC}
done

for i in {42001..42001}
do
	echo "apnsm PushTestDev4 ${ERROR_DEVICE_TOKEN} ${DATE}-hello${i} ${i} chime 86400 {\"key\":${i}}" | ${NC}
done

for i in {43001..43020}
do
	echo "apnsm PushTestDev4 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello${i} ${i} chime 86400 {\"key\":${i}}" | ${NC}
done

# for i in {31000..31005}
# do
#     echo "apnsm PushTestDev3 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello${i} ${i} chime 86400 {\"key\":${i}}" | ${NC}
# done

# for i in {32000..32001}
# do
# 	echo "apnsm PushTestDev3 ${ERROR_DEVICE_TOKEN} ${DATE}-hello${i} ${i} chime 86400 {\"key\":${i}}" | ${NC}
# done

# for i in {33000..33005}
# do
# 	echo "apnsm PushTestDev3 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello${i} ${i} chime 86400 {\"key\":${i}}" | ${NC}
# done

	#echo "apnsm PushTestDev3 ${ERROR_DEVICE_TOKEN} ${DATE}-hello3 3 chime 86400 {\"key\":3}" | ${NC}
	#echo "apnsm PushTestDev4 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello41 41 chime 86400 {\"key\":41}" | ${NC}
	#echo "apnsm PushTestDev3 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello32 32 chime 86400 {\"key\":32}" | ${NC}
	#echo "apnsm PushTestDev4 ${ERROR_DEVICE_TOKEN} ${DATE}-hello4 4 chime 86400 {\"key\":4}" | ${NC}
	#echo "apnsm PushTestDev4 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello42 42 chime 86400 {\"key\":42}" | ${NC}
	#echo "apnsm PushTestDev4 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello43 43 chime 86400 {\"key\":43}" | ${NC}
	#echo "apnsm PushTestDev4 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello44 44 chime 86400 {\"key\":44}" | ${NC}
