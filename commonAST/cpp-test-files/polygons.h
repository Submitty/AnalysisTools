#ifndef __POLYGONS_h__
#define __POLYGONS_h__

#include <iostream>
#include <cassert>
#include <vector>
#include "utilities.h"

using namespace std;

//--------------------------------------------------------------------------------------------
class Polygon{
    public:
        //CONSTRUCTOR
        Polygon(const string& n, const vector<Point>& points);

        //ACCESSOR
        const string& getName() const { return name; } 

        //OTHER FUNCTIONS - virtual because for some classes, computation is not necessary
        //example - right triangles always have a right angle 
        virtual bool HasAllEqualSides() const;
        virtual bool HasAllEqualAngles() const;
        virtual bool HasARightAngle() const;
        virtual bool HasAnObtuseAngle() const;
        virtual bool HasAnAcuteAngle() const;
        virtual bool IsConcave() const; 
        virtual bool IsConvex() const; 

    protected:
        string name;
        vector<Point> verts;
}; 

//--------------------------------------------------------------------------------------------
class Triangle : public Polygon{
    public:
        Triangle(const string& n, const vector<Point>& points) throw(int); 
         
        bool IsConcave() const { return false; }
        bool IsConvex() const { return true; }
};

//--------------------------------------------------------------------------------------------
class ObtuseTriangle : virtual public Triangle{
    public:
        ObtuseTriangle(const string& n, const vector<Point>& points) throw(int);
        bool HasAnObtuseAngle() const { return true; }
        bool HasAllEqualSides() const { return false; }
        bool HasAllEqualAngles() const { return false; }
        bool HasARightAngle() const { return false; }
        bool HasAnAcuteAngle() const { return true; }
};

//--------------------------------------------------------------------------------------------
class IsoscelesTriangle: virtual public Triangle{
    public:
        IsoscelesTriangle(const string& n, const vector<Point>& points) throw(int);
};

//--------------------------------------------------------------------------------------------
class RightTriangle: virtual public Triangle{
    public:
        RightTriangle(const string& n, const vector<Point>& points) throw(int);
        bool HasARightAngle() const { return true; }
        bool HasAnObtuseAngle() const { return false; }
        bool HasAnAcuteAngle() const { return true; }
        bool HasAllEqualAngle() const { return false; }
        bool HasAllEqualSides() const { return false; } 
};

//--------------------------------------------------------------------------------------------
class IsoscelesObtuseTriangle : public ObtuseTriangle, public IsoscelesTriangle{
    public:
        IsoscelesObtuseTriangle(const string& n, const vector<Point>& points) throw(int):
        Triangle(n,points), IsoscelesTriangle(n,points), ObtuseTriangle(n,points) {};
};

//--------------------------------------------------------------------------------------------
class IsoscelesRightTriangle : public RightTriangle, public IsoscelesTriangle{
    public:
        IsoscelesRightTriangle(const string& n, const vector<Point>& points) throw(int):
        Triangle(n,points), IsoscelesTriangle(n,points), RightTriangle(n,points) {};

};

//--------------------------------------------------------------------------------------------
class EquilateralTriangle : public IsoscelesTriangle{
    public:
        EquilateralTriangle(const string& n, const vector<Point>& points) throw(int);
        bool HasAllEqualSides() const { return true; }
        bool HasAllEqualAngles() const { return true; }
        bool HasARightAngle() const { return false; }
        bool HasAnAcuteAngle() const { return true; }
        bool HasAnObtuseAngle() const { return false; }
};

//--------------------------------------------------------------------------------------------
class Quadrilateral: public Polygon{
    public:
        Quadrilateral(const string& n, const vector<Point>& points) throw(int);
};

//--------------------------------------------------------------------------------------------
class Trapezoid: virtual public Quadrilateral{
    public:
        Trapezoid(const string& n, const vector<Point>& points) throw(int);
        bool IsConcave() const { return false; }
        bool IsConvex() const { return true; }
};

//--------------------------------------------------------------------------------------------
class Parallelogram: virtual public Trapezoid{
    public:
        Parallelogram(const string& n, const vector<Point>& points) throw(int);
        
};

//--------------------------------------------------------------------------------------------
class Arrow: public Quadrilateral{
    public:
        Arrow(const string& n, const vector<Point>& points) throw(int);
        bool IsConvex() const { return false; }
        bool IsConcave() const { return true; }
        bool HasAnObtuseAngle() const { return false; }
        bool HasAnAcuteAngle() const { return true; }
        bool HasAllEqualAngles() const { return false; }
        bool HasAllEqualSides() const { return false; }
};


//--------------------------------------------------------------------------------------------
class Kite: virtual public Quadrilateral{
    public:
        Kite(const string& n, const vector<Point>& points) throw(int);
        
        bool IsConvex() const { return true; }
        bool IsConcave() const { return false; }
            
};

//--------------------------------------------------------------------------------------------
class Rhombus: virtual public Parallelogram, public Kite{
    public:
        Rhombus(const string& n, const vector<Point>& points) throw(int); 
        
        bool IsConvex() const { return true; }
        bool IsConcave() const { return false; }
        bool HasAllEqualSides() const { return true; }
};

//--------------------------------------------------------------------------------------------
class IsoscelesTrapezoid: virtual public Trapezoid{
    public:
        IsoscelesTrapezoid(const string& n, const vector<Point>& points) throw(int); 
    
};

//--------------------------------------------------------------------------------------------
class Rectangle: virtual public Parallelogram, public IsoscelesTrapezoid{
    public:
        Rectangle(const string& n, const vector<Point>& points) throw(int); 
       
        bool HasAllEqualAngles() const { return true; }
        bool HasARightAngle() const { return true; }
        bool HasAnObtuseAngle() const { return false; }
        bool HasAnAcuteAngle() const { return false; } 
};

//--------------------------------------------------------------------------------------------
class Square: public Rectangle, public Rhombus{
    public:
        Square(const string& n, const vector<Point>& points) throw(int); 
};

#endif
