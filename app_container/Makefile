build:
	docker build --file=./Dockerfile --tag=dartsviz-pt1 .

run: build
	docker run -d -p 8787:8787  \
		-e DISABLE_AUTH=true \
		--name='dartsviz-pt1' \
		dartsviz-pt1; 


	sleep 3;
	firefox 127.0.0.1:8787;
