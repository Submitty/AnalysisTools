#include "/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw10/polygons.h"
#include "/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw10/utilities.h"

using namespace std;

//-----------------------------------------------

//CONSTRUCTOR
Polygon::Polygon(const string& n, const vector<Point>& points) 
{
    name = n;
    verts = points;
}


bool Polygon::HasAllEqualSides() const
{
    //find the distance between all points and compare them
    for(int i=0; i<verts.size()-2; i++)
    {
        double sideA = DistanceBetween(verts[i], verts[i+1]);
        double sideB = DistanceBetween(verts[i+1], verts[i+2]); 
        if(!EqualSides(sideA, sideB))
        {
            return false;
        } 
    }

    return true;
}

bool Polygon::HasAllEqualAngles() const
{
    //goes through all angles and compares them
    double prevAngle = Angle(verts[0],verts[1],verts[2]);

    int s = verts.size();
    for(int i=0; i<verts.size(); i++)
    {
        double angle = Angle(verts[(i+1)%s],verts[(i+2)%s],verts[(i+3)%s]);

        if(!EqualAngles(angle, prevAngle))
        {
            return false;
        }

        prevAngle = angle;
    } 

    return true;
}

bool Polygon::HasARightAngle() const
{
    //goes through each angle and check if one equals 90 degrees

    int s = verts.size();
    for(int i=0; i<verts.size(); i++)
    {

        double angle = Angle(verts[i],verts[(i+1)%s],verts[(i+2)%s]);

        if(RightAngle(angle))
        {
            return true;
        }
    }

    return false;
}

bool Polygon::HasAnObtuseAngle() const
{
    //goes through each angle and checks if one is > 180 degrees

    int s = verts.size();
    for(int i=0; i<verts.size(); i++)
    {
        double angle;
        angle = Angle(verts[i], verts[(i+1)%s], verts[(i+2)%s]); 

        if(ObtuseAngle(angle))
        {
            return true;
        }

    }

    return false;
}

bool Polygon::HasAnAcuteAngle() const
{
    //goes through each angle and checks to make sure one angle is < 90 

    int s = verts.size();
    for(int i=0; i<verts.size(); i++)
    {
        double angleA = Angle(verts[i],verts[(i+1)%s],verts[(i+2)%s]);

        if(AcuteAngle(angleA))
        {
            return true;
        }
    }

    return false;
}

bool Polygon::IsConvex() const 
{
    //no internal angle is > 180
    int s = verts.size();
    bool prevNeg = false;
    for(int i=0; i<verts.size(); i++)
    {
        double angle = Angle(verts[i], verts[(i+1)%s], verts[(i+2)%s]); 
        bool curNeg = ReflexAngle(angle); 
        if((curNeg != prevNeg) && i!=0)
        {
            return false;            
        }     
        
        prevNeg = curNeg;
    }

    return true;
}

bool Polygon::IsConcave() const
{
    return !IsConvex();
} 

//-----------------------------------------------
Triangle::Triangle(const string& n, const vector<Point>& points) throw(int) : Polygon(n,points)
{
    //must have 3 sides
    if(points.size() != 3)
    {
        throw 1;
    }
} 

//-----------------------------------------------
ObtuseTriangle::ObtuseTriangle(const string& n, const vector<Point>& points) throw(int):
    Triangle(n,points) 
{ 
    //must have at least one obtuse angle
    if(!Polygon::HasAnObtuseAngle())
    {
        throw 1; 
    }
}

//-----------------------------------------------
IsoscelesTriangle::IsoscelesTriangle(const string& n, const vector<Point>& points) throw(int):
    Triangle(n,points) 
{ 

    //must have at least 2 equal sides
    bool twoEqual = false;
    double sideA = DistanceBetween(verts[0], verts[1]);
    double sideB = DistanceBetween(verts[1], verts[2]); 
    double sideC = DistanceBetween(verts[2], verts[0]);

    if(EqualSides(sideA, sideB) || EqualSides(sideB, sideC) || EqualSides(sideC, sideA))
    {
        twoEqual = true; 
    }

    if(!twoEqual)
    {
        throw 1;
    } 
}

//-----------------------------------------------
RightTriangle::RightTriangle(const string& n, const vector<Point>& points) throw(int):
    Triangle(n,points) 
{ 
    //must have a right angle
    if(!Polygon::HasARightAngle())
    {
        throw 1;
    }
}

//-----------------------------------------------
EquilateralTriangle::EquilateralTriangle(const string& n, const vector<Point>& points) throw(int):
    Triangle(n,points),IsoscelesTriangle(n,points) 
{
    //must have all equal sides
    if(!Polygon::HasAllEqualSides())
    {
        throw 1;
    }
}

//-----------------------------------------------
Quadrilateral::Quadrilateral(const string& n, const vector<Point>& points) throw(int): Polygon(n,points) 
{
    //must have 4 sides
    if(points.size() != 4)
    {
        throw 1;
    }
}

//-----------------------------------------------
Trapezoid::Trapezoid(const string& n, const vector<Point>& points) throw(int): Quadrilateral(n,points) 
{
    Vector SideA(points[0], points[1]);
    Vector SideB(points[1], points[2]); 
    Vector SideC(points[2], points[3]);
    Vector SideD(points[3], points[0]);

    //must have a pair of parallel sides
    if(!(Parallel(SideA,SideC) || Parallel(SideB,SideD)))
    {
        throw 1;
    }    
}

//-----------------------------------------------
Parallelogram::Parallelogram(const string& n, const vector<Point>& points) throw(int):
    Quadrilateral(n,points), Trapezoid(n,points) 
{
    //make sure there are a parallel sides

    Vector SideA(points[0], points[1]);
    Vector SideB(points[1], points[2]); 
    Vector SideC(points[2], points[3]);
    Vector SideD(points[3], points[0]);

    if(!Parallel(SideA,SideC) || !Parallel(SideB,SideD))
    {
        throw 1;
    }
}

//-----------------------------------------------
double getSlope(double x1, double x2, double y1, double y2)
{
    //divide by zero case
    if((x1-x2) == 0)
    {
        return 0;
    }

    //dy/dx
    return ((y1-y2)/(x1-x2));
}

double reciprocal(double slope)
{
    //divide by zero case
    if(slope == 0)
    {
        return 0; 
    }

    return 0-(1/slope);
}

double midpoint(double a, double b)
{
    return double(a+b)/2.0;
}

//-----------------------------------------------
Arrow::Arrow(const string& n, const vector<Point>& points) throw(int): Quadrilateral(n,points) 
{
    double sideA = DistanceBetween(verts[0], verts[1]);
    double sideB = DistanceBetween(verts[1], verts[2]); 
    double sideC = DistanceBetween(verts[2], verts[3]);
    double sideD = DistanceBetween(verts[3], verts[0]); 

    //must have two pairs of equal sides 
    if(!(EqualSides(sideA, sideB) && EqualSides(sideC, sideD) ||
                (EqualSides(sideD, sideA) && EqualSides(sideB, sideC))))
    {
        throw 1;
    }

    double angleA = Angle(verts[0], verts[1], verts[2]);
    double angleB = Angle(verts[2], verts[3], verts[0]);
    double angleC = Angle(verts[3], verts[0], verts[1]);
    double angleD = Angle(verts[1], verts[2], verts[3]);

    //at least one angle must be greater than 180
    if(!(angleA > 180 || angleB > 180 || angleC > 180 || angleD > 180))
    {
        throw 1;
    }
}

//-----------------------------------------------
Kite::Kite(const string& n, const vector<Point>& points) throw(int): Quadrilateral(n,points) 
{
    double sideA = DistanceBetween(verts[0], verts[1]);
    double sideB = DistanceBetween(verts[1], verts[2]); 
    double sideC = DistanceBetween(verts[2], verts[3]);
    double sideD = DistanceBetween(verts[3], verts[0]); 

    //must have two pairs of equal sides 
    if(!(EqualSides(sideA, sideB) && EqualSides(sideC, sideD) ||
                (EqualSides(sideD, sideA) && EqualSides(sideB, sideC))))
    {
        throw 1;
    }

    double angleA = Angle(verts[0], verts[1], verts[2]);
    double angleB = Angle(verts[2], verts[3], verts[0]);
    double angleC = Angle(verts[3], verts[0], verts[1]);
    double angleD = Angle(verts[1], verts[2], verts[3]);


    //must have equal angles between the two equal sides
    if(!(EqualAngles(angleA, angleB) || EqualAngles(angleC, angleD)))
    {
        throw 1;
    }

    //get the slopes of the diagonals
    double slopeA = getSlope(verts[0].x,verts[2].x,verts[0].y,verts[2].y);
    double slopeB = getSlope(verts[1].x,verts[3].x,verts[1].y,verts[3].y); 

    //must have diagonals that create a right angle
    //reciprocal creates perpendicular lines
    if(!(slopeA == reciprocal(slopeB))) 
    {
        throw 1;
    }

    double xMidA = midpoint(verts[0].x,verts[2].x);
    double xMidB = midpoint(verts[1].x,verts[3].x);
    double yMidA = midpoint(verts[0].y,verts[2].y);
    double yMidB = midpoint(verts[1].y,verts[3].y);

    //diagonals must bisect
    //vertical and horizontal diagonals case
    double dxA = verts[0].x - verts[2].x;
    double dxB = verts[1].x - verts[3].x;
    double dyA = verts[0].y - verts[2].y;
    double dyB = verts[1].y - verts[3].y;

    if(dxA == 0 || dxB == 0)
    {
        if(xMidA != xMidB) 
        {
            throw 1;
        }
    }
    //use point slope formula
    else if(!(yMidB == ((slopeA*(xMidB - xMidA)) + yMidA) ||
                yMidA == ((slopeB*(xMidA - xMidB)) + yMidB)))
    {
        throw 1;
    } 
}


//-----------------------------------------------
Rhombus::Rhombus(const string& n, const vector<Point>& points) throw(int): 
    Quadrilateral(n,points), Trapezoid(n,points), Kite(n,points), Parallelogram(n,points) 
{
    //must have all equal sides
    if(!Polygon::HasAllEqualSides())
    {
        throw 1;
    }
}

//-----------------------------------------------
IsoscelesTrapezoid::IsoscelesTrapezoid(const string& n, const vector<Point>& points) throw(int): 
    Quadrilateral(n,points), Trapezoid(n,points) 
{
    Vector SideA(points[0], points[1]);
    Vector SideB(points[1], points[2]); 
    Vector SideC(points[2], points[3]);
    Vector SideD(points[3], points[0]);

    //the sides that aren't parallel must be equal in length 
    if((!Parallel(SideA,SideC) && 
                !EqualSides(DistanceBetween(points[0],points[1]),DistanceBetween(points[2],points[3]))) || 
            (!Parallel(SideB,SideD) && 
             !EqualSides(DistanceBetween(points[1],points[2]),DistanceBetween(points[3],points[0]))))
    {
        throw 1;
    }

    //both angles coming from parallel sides must be equal 

    double angleA = Angle(points[0], points[1], points[2]);
    double angleB = Angle(points[1], points[2], points[3]);
    double angleC = Angle(points[2], points[3], points[0]);
    double angleD = Angle(points[3], points[0], points[1]);

    if((Parallel(SideA,SideC) && !EqualAngles(angleA,angleB)) || 
            (Parallel(SideB,SideD) && !EqualAngles(angleC,angleD)))
    {
        throw 1;
    } 

}

//-----------------------------------------------
Rectangle::Rectangle(const string& n, const vector<Point>& points) throw(int): 
    Quadrilateral(n,points), Parallelogram(n,points), 
    Trapezoid(n,points), IsoscelesTrapezoid(n,points)             
{
    //must have all right angles

    double angleA = Angle(points[0], points[1], points[2]);
    double angleB = Angle(points[1], points[2], points[3]);
    double angleC = Angle(points[2], points[3], points[0]);
    double angleD = Angle(points[3], points[0], points[1]);

    if(!RightAngle(angleA) || !RightAngle(angleB) || !RightAngle(angleC) || !RightAngle(angleD))
    {
        throw 1;
    }

}

//-----------------------------------------------
Square::Square(const string& n, const vector<Point>& points) throw(int): 
    Quadrilateral(n,points), Parallelogram(n,points), Trapezoid(n,points), 
    Rectangle(n, points), Rhombus(n,points)
{
    //mush have all equal sides
    if(!Polygon::HasAllEqualSides())
    {
        throw 1;
    } 

}

