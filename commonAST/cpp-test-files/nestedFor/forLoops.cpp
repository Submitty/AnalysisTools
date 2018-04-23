using namespace std;

int addOne(int b){
	return b+1;
}

int main(){

	int d = 1;
	for(int i=0; i<10; i++){
		d = addOne(d);
	}

	for(int i=10; i>=0; i--){
		d--;
	}

	for(int i=0; i<4; i++){
	 	d+=2;
	}

	bool flag = true;
	while(flag){
		if(d==10){
			flag = true;	
		}else if(d==9){
			d=10;
		}else{
			continue;
		}

		d = addOne(d);
	}
}
