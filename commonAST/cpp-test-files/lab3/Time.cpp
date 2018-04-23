#include "Time.h"
#include <iostream>

using namespace std;

Time::Time()
{
	hour = 0;
	minute = 0;
	second = 0;

}

Time::Time(int aHour, int aMinute, int aSecond)
{
	hour = aHour;
	minute = aMinute;
	second = aSecond;
}

int Time::getSecond() const
{
	return second;
}

int Time::getMinute() const
{
	return minute;
}

int Time::getHour() const
{
	return hour;
}

void Time::setHour(int newHour)
{
	hour = newHour;
}

void Time::setMinute(int newMinute)
{
	minute = newMinute;
}

void Time::setSecond(int newSecond)
{
	second = newSecond;
}

void Time::printAmPm()
{
	if(hour > 12)
	{

		cout << hour-12 << ":";

		if(minute<10)
		{
			cout << "0";
		}
		cout << minute << ":";
		if(second<10)
		{
			cout << "0";
		}
		
		cout << second << "pm" << endl;

	}
	else
	{

		if(hour == 0)
		{
			setHour(12);
		}
		cout << hour << ":";
		if(minute<10)
		{
			cout << "0";
		}
		cout << minute << ":";
		if(second<10)
		{
			cout << "0";
		}
		
		cout << second << "am" << endl;
	}
}

	bool isEarlierThan(const Time& t1, const Time& t2)
	{
		if (t1.getHour() < t2.getHour())
		{
			return true;
		}
		else if(t2.getHour() < t1.getHour())
		{
			return false;
		}
		else{
			if(t1.getMinute() < t2.getMinute())
			{
				return true;
			}
			else if(t2.getMinute() < t1.getMinute())
			{
				return false;
			}
			else{
				if(t1.getSecond() < t2.getSecond())
				{
					return true;
				}
				else
				{
					return false;
				}
			}
		}

	}
