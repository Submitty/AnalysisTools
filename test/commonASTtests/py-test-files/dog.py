class Animal:
	def __init__(self, _name, _age):
		self.name = _name;
		self.age = _age;

	def birthday(self):
		self.age += 1; 

class Dog(Animal):
	def __init__(self, _name, _age, _breed):
		Animal.__init__(self, _name, _age)
		self.breed = _breed

	def bark(self):
		print self.breed
		print "bark!"	
		

class GermanShepard(Dog):
	def __init__(self, _name, _age):
		Dog.__init__(self, _name, _age, "GermanShepard")

d = Dog("n", 10, "poodle")
d.bark()
