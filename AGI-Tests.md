# Sipp phone registration

sipp 127.0.0.1 -sf REGISTER_client.xml -inf REGISTER_client.csv -m 1 -l 1 -trace_msg -trace_err

http://tomeko.net/other/sipp/sipp_cheatsheet.php?lang=en

# pjsua

## Multiple listeners

./pjsua-x86_64-unknown-linux-gnu --local-port=5061 --registrar=sip:127.0.0.1:5060 --id=sip:40@127.0.0.1:5060 --realm=* --username=40 --password=123456 --next-account --registrar=sip:127.0.0.1:5060 --id=sip:50@127.0.0.1:5060 --realm=* --username=50 --password=123456 --next-account --registrar=sip:127.0.0.1:5060 --id=sip:60@127.0.0.1:5060 --realm=* --username=60 --password=123456 --null-audio --play-file /root/sound.wav --auto-answer 200 --auto-play

## Call originated by pjsua

./pjsua-x86_64-unknown-linux-gnu --local-port=5065 --registrar=sip:127.0.0.1:5060 --id=sip:10@127.0.0.1:5060 --realm=* --username=10 --password=123456 sip:50@127.0.0.1 --null-audio --auto-play --auto-loop

## Refs

http://stackoverflow.com/questions/16483635/pjsip-new-call-error-unable-to-find-default-audio-device-pjmedia-eaud-nodef
