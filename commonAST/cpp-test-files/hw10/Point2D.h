#ifndef Point2D_h_
#define Point2D_h_

class Point2D {

public:
  // constructors
  Point2D() : m_x(0), m_y(0) {}
  Point2D( float x, float y) 
    : m_x(x), m_y(y) {}

  // accessors
  float x() const { return m_x; }
  float y() const { return m_y; }

  // modifier
  float set( float x, float y )
    { m_x = x; m_y = y; }

private:
  // representation
  float m_x, m_y;
};

#endif
