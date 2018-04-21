#include <iostream>
#include "Rectangle.h"
#include <vector>

using namespace std;

int main(){
    
    //create and print rectangles

    Point2D p1(1,3);
    Point2D p2(5,10);

    Point2D p3(1,1);
    Point2D p4(2,15);

    Point2D p5(50,1);
    Point2D p6(51,100);

    Point2D p7(4,4);
    Point2D p8(53,4);

    Rectangle rect1(p1,p2);
    Rectangle rect2(p3,p4);
    Rectangle rect3(p5,p6);
    //rect4 has intentionally wrong points
    Rectangle rect4(p8,p3);

    print_rectangle(rect1);
    print_rectangle(rect2);
    print_rectangle(rect3);
    print_rectangle(rect4);

    //test is_point_within
    cout << "RECT 1" << endl;
    cout << rect1.is_point_within(p5) << endl;
    cout << rect1.is_point_within(p1) << endl;
    cout << rect1.is_point_within(p7) << endl;
    cout << "RECT 2" <<endl;
    cout << rect2.is_point_within(p1) << endl;
    cout << rect2.is_point_within(p6) << endl;
    cout << rect2.is_point_within(p4) << endl;
    cout << "RECT 3" << endl;
    cout << rect3.is_point_within(p5) << endl;
    cout << rect3.is_point_within(p8) << endl;
    cout << "RECT 4" << endl;
    cout << rect4.is_point_within(p5) << endl;
    cout << rect4.is_point_within(p8) << endl;


    //test add_point
    rect1.add_point(p8);
    rect1.add_point(p1);
    rect1.add_point(p4);
    rect1.add_point(p6);
    
    rect2.add_point(p8);
    rect2.add_point(p7);
    rect2.add_point(p1);

    rect3.add_point(p1);
    rect3.add_point(p8);

    vector<Point2D> pointsContained1;
    pointsContained1 = rect1.points_contained();

    cout << "points contained in rect1" << endl;
    for(int i=0; i<pointsContained1.size(); i++)
    {
        cout << pointsContained1[i].x() << ", " << pointsContained1[i].y()  << endl;
    }

    cout <<  "points contained in rect2" << endl;
    vector<Point2D> pointsContained2;
    pointsContained2 = rect2.points_contained();
    for(int i=0; i<pointsContained2.size(); i++)
    {
        cout << pointsContained2[i].x() << ", " << pointsContained2[i].y() <<  endl;
    }
    
    cout << "points contained in rect3" << endl;
    vector<Point2D> pointsContained3;
    pointsContained3 = rect3.points_contained();
    for(int i=0; i<pointsContained3.size(); i++)
    {
        cout << pointsContained3[i].x() << ", " << pointsContained3[i].y()  << endl;
    }

    return 0;
}
