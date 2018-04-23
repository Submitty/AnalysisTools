int main(){
	bool a = false;
	bool b;
	int k = 0;
	if(k==0){
		k = k+1;
		a = true;
		b = !a && a || a && a;
	}else if(k>0){
		k = k+2;	
	}else if(k<0){
		k = k+3;
	}else if(!b){
		k = 1;
	}else{
		b = false;
	}

}
