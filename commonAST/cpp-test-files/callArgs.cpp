int addOne(int a){
	return a+1;
}

int subOne(int a){
	return a-1;
}

int main(){
	int a = 0;
	int b = subOne(addOne(a));
}
