#include <iostream>

using namespace std;

class Animal{
	public:
		Animal(){
			name = "";
			age = 0;
		}

		Animal(string n, int a){
			name = n;	
			age = a;
		}		

		void birthday(){
			age += 1;
		}

	private:
		string name;
		int age;		
};

class Dog : public Animal{
	public:
		Dog(string n, int a, string b){
			Animal(n, a);
			breed = b;
		}

		Dog(){
			Animal();
			breed = "";
		}

	 	void bark(){
			cout << breed << endl;
			cout << "bark" << endl;
		}
	private:
		string breed;
		
};

class GermanShepard : public Dog{
	public:
		GermanShepard(string n, int a){
			Dog(n,a,"GermanShepard");
		}
};

int main(){
	Dog d("princess", 10, "husky");
	d.bark();

	Dog* e = new Dog("name", 5, "GoldenRetriever");
	e->bark();
}
