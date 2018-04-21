class Time{

private:
	int hour;
	int minute;
	int second;

public:
	Time();
	Time(int aHour, int aMinute, int aSecond);

	//ACCESSORS
	int getHour() const;
	int getMinute() const;
	int getSecond() const;

	//MODIFIERS
	void setHour(int newHour);
	void setMinute(int newMinute);
	void setSecond(int newSecond);

	//OTHER
	void printAmPm();
};

bool isEarlierThan(const Time& t1, const Time& t2);