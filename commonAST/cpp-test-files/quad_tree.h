
//
// We provide the Point class and the implementation of several
// QuadTree member functions for printing.  
//
// IMPORTANT: You should modify this file to add all of the necessary
// functionality for the QuadTree class and its helper classes: Node,
// DepthIterator, and BreadthIterator.
//
// ===================================================================

#ifndef quad_tree_h_
#define quad_tree_h_

#include <math.h>
#include <algorithm>
#include <iostream>
#include <vector>
#include <cassert>
#include <queue>

using namespace std;

// ==============================================================
// ==============================================================
// A tiny templated class to store 2D coordinates.  This class works
// with number_type = int, float, double, unsigned char (a 1 byte=8
// bit integer), short (a 2 byte=16 bit integer).

template <class number_type>
class Point {
    public:
        Point() { x = 0; y = 0; };
        Point(const number_type& x_, const number_type& y_) : x(x_),y(y_) {}
        // REPRESENTATION
        number_type x;
        number_type y;


}; 

// a helper function to print Points to STL output stream
template <class number_type>
inline std::ostream& operator<<(std::ostream &ostr, const Point<number_type> &pt) {
    ostr << "(" << pt.x << "," << pt.y << ")";
    return ostr;
};

template <class number_type, class label_type>
class Node
{
    public:
        //CONSTRUCTORS
        Node()
        {
            for(int i=0; i<4; i++)
            {
                children[i] = NULL;
            } 

            parent = NULL;
        };

        Node(const label_type& l, const Point<number_type>& p, Node<number_type, label_type>* prnt)
        {

            for(int i=0; i<4; i++)
            {
                children[i] = NULL;
            }

            label = l; 
            pt = p; 
            parent = prnt; 
        };

        //MEMBER VARIABLES
        Point<number_type> pt;
        label_type label;
        Node<number_type, label_type>* children[4];
        Node<number_type, label_type>* parent;
};

template<class number_type, class label_type>
class BFiterator
{
    public:
        //CONSTRUCTOR
        BFiterator(Node<number_type, label_type>* ptr) { p = ptr; };

        //ACCESSORS
        label_type getLabel() const { if(p) {return p->label;} };
        int getDepth() const
        {
            int level = -1;
            Node<number_type, label_type>* tmp =p; 
            while(tmp != NULL)
            {
                level++;
                tmp = tmp->parent; 
            }
            return level;
        };  

        //OPERATORS
        Point<number_type>& operator*() { return p->pt; };
        bool operator==(const BFiterator<number_type, label_type>& itr2) { return p == itr2.p; }; 
        bool operator!=(const BFiterator<number_type, label_type>& itr2) { return not (p == itr2.p); };

        //goes "accross tree"
        BFiterator operator++()
        {
            //adds all children to queue
            for(int i=0; i<4; i++)
            {
                if(p->children[i] != NULL)
                {
                    q.push(p->children[i]);
                }
            } 

            //pulls first one off queue
            if(q.empty())
            {
                p = NULL; 
            }        
            else
            {
                p = q.front();
                q.pop();
            }
            return *this;
        }

        //same as above but returns the unmodified pointer
        BFiterator operator++(int)
        {
            Node<number_type, label_type>* old;

            for(int i=0; i<4; i++)
            {
                if(p->children[i] != NULL)
                {
                    q.push(p->children[i]);
                }
            } 

            old = p;
            if(q.empty())
            {
                p = NULL;    
            }
            else 
            {
                p = q.front();
                q.pop();
            }

            return BFiterator(old); 
        }

    private:
        //MEMBER VARIABLES
        Node<number_type, label_type>* p; 
        queue< Node<number_type, label_type>* > q;
};

template<class number_type, class label_type>
class DepthIterator
{
    public:
        //CONSTRUCTOR
        DepthIterator(Node<number_type, label_type>* ptr) { p = ptr; };
        DepthIterator() { p = NULL; };

        //ACCESSORS
        label_type getLabel() const { if(p) { return p->label; } };

        //traverses up the tree and increments each time to find depth 
        int getDepth() const
        {
            int level = -1;
            Node<number_type, label_type>* tmp =p; 
            while(tmp != NULL)
            {
                level++;
                tmp = tmp->parent; 
            }
            return level;
        };

        //OPERATORS
        Point<number_type>& operator*() { return p->pt; };
        bool operator==(const DepthIterator<number_type, label_type>& itr2) { return p == itr2.p; }; 
        bool operator!=(const DepthIterator<number_type, label_type>& itr2) { return not (p == itr2.p); };

        //increments "down" the tree
        DepthIterator operator++() 
        {
            if(p->children[0] != NULL or p->children[1] != NULL or
                    p->children[2] != NULL or p->children[3] != NULL)    
                //not yet at bottom of tree
            {
                for(int i=0; i<4; i++)
                {
                    if(p->children[i] != NULL)
                    {
                        p = p->children[i];
                        break;
                    }
                }         
            }
            else
                //reached a leaf node
            {

                //determine which child you are and go to the next child
                //if you are the last valid child, then become the parent 
                //and determine which child you are and go to the next child 

                while(p != NULL and amLastChild(p->parent, p))
                {
                    p = p->parent; 
                }

                if(p != NULL and p->parent != NULL)   
                {
                    p = p->parent->children[getNextChild(p->parent,p)];
                }

            }

            return *this; 
        }; 


        DepthIterator operator++(int) 
        {
            Node<number_type, label_type>* old = p;
            if(p->children[0] != NULL or p->children[1] != NULL or
                    p->children[2] != NULL or p->children[3] != NULL)    
                //not yet at bottom of tree
            {
                for(int i=0; i<4; i++)
                {
                    if(p->children[i] != NULL)
                    {
                        p = p->children[i];
                        break;
                    }
                }         
            }
            else
                //reached a leaf node
            {

                //determine which child you are and go to the next child
                //if you are the last valid child, then become the parent 
                //and determine which child you are and go to the next child 

                while(p != NULL and amLastChild(p->parent, p))
                {
                    p = p->parent; 
                }

                if(p != NULL and p->parent != NULL)   
                {
                    p = p->parent->children[getNextChild(p->parent,p)];
                }

            }

            return DepthIterator(old); 
        };


    private:
        Node<number_type, label_type>* p; 
        //HELPER FUNCTIONS

        //helper function to return int of which child node n is
        //returns 4 if not a child
        int whichChild(const Node<number_type, label_type>* parent, const Node<number_type, label_type>* n) const
        {
            for(int i=0; i<4; i++)
            {
                if(parent != NULL and parent->children[i] == n)
                {
                    return i;
                }
            }

            return 4;
        }

        //helper function to return the next child
        //returns 4 if last child 
        int getNextChild(const Node<number_type, label_type>* parent, const Node<number_type, label_type>* n) const
        {
            if(amLastChild(parent, n))
            {
                return 4;
            }

            int child = whichChild(parent, n);
            for(int i=child+1; i<4; i++)
            {
                if(parent->children[i] != NULL)
                {
                    return i;
                }
            }   
        }

        //helper function for incrementing. Checks if is the last child of its parent
        bool amLastChild(const Node<number_type, label_type>* parent, const Node<number_type, label_type>* n) const
        {

            if(parent == NULL)
            {
                return true;
            }

            int child = whichChild(parent, n);
            for(int i=child+1; i<4; i++)
            {
                if(parent->children[i] != NULL)
                {
                    return false;
                }
            } 

            return true; 
        }
};

template <class number_type, class label_type>
class QuadTree
{
    public:
        //CONSTRUCTORS AND DESTRUCTOR 
        QuadTree() { root_ = NULL; size_ = 0; };
        ~QuadTree() { this->destroy_tree(root_); root_ = NULL; }; 
        QuadTree(const QuadTree<number_type, label_type>& old) 
        { 
            root_ = NULL;
            this->copy_tree(NULL, root_, old.root_); 
            size_ = old.size_; 
        };

        //ASSIGNMENT OPERATOR
        QuadTree& operator=(const QuadTree<number_type, label_type>& old)
        {
            if(&old != this)
            {
                this->destroy_tree(root_);
                this->copy_tree(NULL, root_, old.root_);
                size_ = old.size_;
            }

            return *this;
        }        

        //ITERATOR TYPEDEFS
        typedef DepthIterator<number_type, label_type> iterator;
        typedef BFiterator<number_type, label_type> bf_iterator;

        //ACCESSORS
        int size() const { return size_; };
        int height() const { 
            Node<number_type, label_type>* p = root_;
            return heightHelper(p); 
        };

        iterator begin() const { return iterator(root_); };
        iterator end() const { return iterator(NULL); };
        bf_iterator bf_begin() const { return bf_iterator(root_); };
        bf_iterator bf_end() const { return bf_iterator(NULL); };


        //OTHER FUNCTIONS
        iterator find(const number_type& x, const number_type& y) { return find(x,y,root_); };
        pair<iterator, bool> insert(const Point<number_type>& pnt, const label_type& label)
        {
            size_++; 
            return insert(pnt,label,root_, NULL); 
        };



        // ==============================================================
        // PROVIDED CODE : QUAD TREE MEMBER FUNCTIONS FOR PRINTING
        // ==============================================================

        // NOTE: this function only works for quad trees with non negative
        // integer coordinates and char labels

        // NOTE2: this function assumes that no two points have the same x
        // coordinate or the same y coordinate.

        // plot driver function
        // takes in the maximum x and y coordinates for these data points
        // the optional argument draw_lines defaults to true
        void plot(int max_x, int max_y, bool draw_lines=true) const {
            // allocate blank space for the center of the board
            std::vector<std::string> board(max_y+1,std::string(max_x+1,' '));
            // edit the board to add the point labels and draw vertical and
            // horizontal subdivisions
            plot(root_,board,0,max_x,0,max_y,draw_lines);
            // print the top border of the plot
            std::cout << "+" << std::string(max_x+1,'-') << "+" << std::endl;
            for (int i = 0; i <= max_y; i++) {
                // print each row of the board between vertical border bars
                std::cout << "|" << board[i] << "|" << std::endl;
            }
            // print the top border of the plot
            std::cout << "+" << std::string(max_x+1,'-') << "+" << std::endl;
        }

        // actual recursive function for plotting
        void plot(Node<number_type,label_type> *p, std::vector<std::string> &board,
                int x_min, int x_max, int y_min, int y_max, bool draw_lines) const {
            // base case, draw nothing if this node is NULL
            if (p == NULL) return;
            // check that the dimensions range of this node make sense
            assert (x_min >= 0 && x_min <= x_max);
            assert (y_min >= 0 && y_min <= y_max);
            assert (board.size() >= y_max);
            assert (board[0].size() >= x_max);
            // verify that the point stored at this node fits on the board
            assert (p->pt.y >= 0 && p->pt.y < board.size());
            assert (p->pt.x >= 0 && p->pt.x < board[0].size());
            // draw the vertical and horizontal bars extending across the
            // range of this node
            if (draw_lines) {
                for (int x = x_min; x <= x_max; x++) {
                    board[p->pt.y][x] = '-';
                }
                for (int y = y_min; y <= y_max; y++) {
                    board[y][p->pt.x] = '|';
                }
            }
            // draw this label
            board[p->pt.y][p->pt.x] = p->label;
            // recurse on the 4 children
            plot(p->children[0],board,x_min ,p->pt.x-1,y_min ,p->pt.y-1,draw_lines);
            plot(p->children[1],board,p->pt.x+1,x_max ,y_min ,p->pt.y-1,draw_lines);
            plot(p->children[2],board,x_min ,p->pt.x-1,p->pt.y+1,y_max ,draw_lines);
            plot(p->children[3],board,p->pt.x+1,x_max ,p->pt.y+1,y_max ,draw_lines);
        }

        // ==============================================================

        // prints all of the tree data with a pre-order (node first, then
        // children) traversal of the tree structure

        // driver function
        void print_sideways() const { print_sideways(root_,""); }

        // actual recursive function
        void print_sideways(Node<number_type,label_type>* p, const std::string &indent) const {
            // base case
            if (p == NULL) return;
            // print out this node
            std::cout << indent << p->label << " (" << p->pt.x << "," << p->pt.y << ")" << std::endl;
            // recurse on each of the children trees
            // increasing the indentation
            print_sideways(p->children[0],indent+"  ");
            print_sideways(p->children[1],indent+"  ");
            print_sideways(p->children[2],indent+"  ");
            print_sideways(p->children[3],indent+"  ");
        }

        // ==============================================================
        // ==============================================================

    private:
        //MEMBER VARIABLES
        Node<number_type, label_type>* root_;
        unsigned int size_;

        //HELPER FUNCTIONS
        void destroy_tree(Node<number_type, label_type>*& p)
        {
            if(p == NULL)
            {
                return;
            }

            //recurses on each child to delete each node
            destroy_tree(p->children[0]);
            destroy_tree(p->children[1]);
            destroy_tree(p->children[2]);
            destroy_tree(p->children[3]);
            delete p;

        }

        //recursive function that inserts a node and returns a pair with first being a depth first iterator and 
        //the second value a bool. true if inserted and false if not
        pair<iterator,bool> insert(const Point<number_type>& pnt, const label_type& label, 
                Node<number_type, label_type>*& p, Node<number_type, label_type>* parent)
        {

            if(pnt.x < 0 or pnt.y < 0)
            {
                return make_pair(iterator(NULL),false);
            }

            if(!p)
            {
                p = new Node<number_type, label_type>(label, pnt, parent); 
                return make_pair(iterator(p),true); 
            }
            //top left quadrant
            else if(pnt.x < p->pt.x and pnt.y < p->pt.y)
            {
                return insert(pnt, label, p->children[0],p);
            }
            //top right quadrant
            else if(pnt.x > p->pt.x and pnt.y < p->pt.y) 
            {
                return insert(pnt, label, p->children[1],p);
            }
            //bottom left quadrant
            else if(pnt.x < p->pt.x and pnt.y > p->pt.y)
            {
                return insert(pnt, label, p->children[2],p);
            }
            //bottom right quadrant
            else if(pnt.x > p->pt.x and pnt.y > p->pt.y)
            {
                return insert(pnt, label, p->children[3],p);
            }
            else
            {
                //if point already in tree
                if (pnt.x == p->pt.x and pnt.y == p->pt.y)
                {
                    size_--;
                    return make_pair(iterator(p), false);
                }
            }
        }

        //recursive function to find point. Returns an iterator to end if not found
        iterator find(const number_type& x, const number_type& y, Node<number_type, label_type>* p)
        {

            //point not in tree
            if(!p)
            {
                return iterator(NULL);
            }   
            //found it
            else if(p->pt.x == x and p->pt.y == y)
            {
                return iterator(p); 
            }
            //top left quadrant
            else if(x < p->pt.x and y < p->pt.y)
            {
                return find(x, y, p->children[0]);
            }
            //top right quadrant
            else if(x > p->pt.x and y < p->pt.y) 
            {
                return find(x, y, p->children[1]);
            }
            //bottom left quadrant
            else if(x < p->pt.x and y > p->pt.y)
            {
                return find(x, y, p->children[2]);
            }
            //bottom right quadrant
            else if(x > p->pt.x and y > p->pt.y)
            {
                return find(x, y, p->children[3]);
            }
        }

        //recursive function to find height  
        int heightHelper(Node<number_type, label_type>* p) const
        {
            if(p == NULL)
            {
                return -1;
            }

            return 1 + max(max(heightHelper(p->children[0]), heightHelper(p->children[1])), 
                    max(heightHelper(p->children[2]), heightHelper(p->children[3]))); 
        }

        //recursive function to copy tree. recurses on each child
        void copy_tree(Node<number_type, label_type>* parent, Node<number_type, label_type>*& _new, 
                Node<number_type, label_type>* old)
        {
            if(old != NULL)
            {
                _new = new Node<number_type, label_type>(old->label, old->pt, parent);
                copy_tree(_new, _new->children[0], old->children[0]);
                copy_tree(_new, _new->children[1], old->children[1]);
                copy_tree(_new, _new->children[2], old->children[2]);
                copy_tree(_new, _new->children[3], old->children[3]);
            } 
            else
            {
                _new = NULL;
            }

        }

};

template<typename number_type, number_type x, number_type y>
bool distance(const Point<number_type>& p1, const Point<number_type>& p2)
{
    return sqrt((p1.x * p1.x) + (p1.y * p1.y)) < sqrt((p2.x * p2.x) + (p2.y * p2.y)); 
}

template<typename number_type>
void find_middle(iterator first, iterator last, max x, max y)
{
    if(first == last)
    {
        return;
    }

    sort(first, last, distance<number_type, max x, max y>);
}

template<typename number_type, typename label_type>
void BalanceTree(vector< pair<Point<number_type>,label_type> >& points)
{
    if(points.size() == 0)
    {
        return;
    }

    sort(points.begin(), points.end(), pairCompare<number_type,label_type>); 

    vector< pair<Point<number_type>, label_type> > newPoints;
    for(int i=0; i<points.size()/2; i++)
    {
        newPoints.push_back(points[points.size()/2-i]);
        if(i!=0)
        {
            newPoints.push_back(points[points.size()/2+i]);
        } 
    }

    points = newPoints;
}

#endif
