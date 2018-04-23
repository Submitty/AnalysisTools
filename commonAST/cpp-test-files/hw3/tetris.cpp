#include <iostream>
#include <algorithm>
#include <cstdlib>
#include "tetris.h"

using namespace std;

const char Tetris::I[1][4] = {{'I','I','I','I'}};
const char Tetris::O[2][2] = {{'O','O'},{'O','O'}};
const char Tetris::T[3][2] = {{' ','T'},{'T','T'},{' ','T'}}; 
const char Tetris::Z[3][2] = {{' ','Z'},{'Z','Z'},{'Z',' '}};
const char Tetris::S[3][2] = {{'S',' '},{'S','S'},{' ','S'}};
const char Tetris::L[2][3] = {{'L','L','L'},{'L',' ',' '}};
const char Tetris::J[2][3] = {{'J',' ',' '},{'J','J','J'}};

Tetris::Tetris(int w)
{
    if(w <= 0)
    {
        cerr << "can not create a board of size " << w << endl;
        destroy();
        exit(1);
    }

	width = w;
    heights = new int[width];
    //initializes all heights to 0
    for(int i=0;i<w;i++)
    {
        heights[i] = 0;
    }
    
    data = new char* [width];
    for(int i=0; i<width; i++)
    {
        data[i] = NULL;
    }
}

void Tetris::add_piece(char letter, int rotation, int x)
{
    char** pieceToAdd;
    //switch statements for each letter that give values to 
    //the pieceLength, pieceHeight, and fill the pieceToAdd 2D array

    int pieceLength, pieceHeight, maxHeight;
    switch (letter)
    {
        case 'I':
            pieceLength = 1;
            pieceHeight = 4;

            pieceToAdd = new char*[pieceLength]; 

            for(int i=0; i<pieceLength; i++)
            {
                    pieceToAdd[i] = new char[pieceHeight];
            }

            for(int i=0; i<pieceLength; i++)
            {
                for(int j=0; j<pieceHeight; j++)
                {
                    pieceToAdd[i][j] = I[i][j];
                }
            }

            break;

        case 'O':
            pieceLength = 2;
            pieceHeight = 2;
       
            pieceToAdd = new char*[pieceLength]; 

            for(int i=0; i<pieceLength; i++)
            {
                    pieceToAdd[i] = new char[pieceHeight];
            }

            for(int i=0; i<pieceLength; i++)
            {
                for(int j=0; j<pieceHeight; j++)
                {
                    pieceToAdd[i][j] = O[i][j];
                }
            }

            break;

        case 'T':
            pieceLength = 3;
            pieceHeight = 2;
           
            pieceToAdd = new char*[pieceLength]; 

            for(int i=0; i<pieceLength; i++)
            {
                    pieceToAdd[i] = new char[pieceHeight];
            }

            for(int i=0; i<pieceLength; i++)
            {
                for(int j=0; j<pieceHeight; j++)
                {
                    pieceToAdd[i][j] = T[i][j];
                }
            
            }

            break;

        case 'Z':
            pieceLength = 3;
            pieceHeight = 2;

            pieceToAdd = new char*[pieceLength]; 

            for(int i=0; i<pieceLength; i++)
            {
                    pieceToAdd[i] = new char[pieceHeight];
            }

            for(int i=0; i<pieceLength; i++)
            {
                for(int j=0; j<pieceHeight; j++)
                {
                    pieceToAdd[i][j] = Z[i][j];
                }
            
            }
            break;

        case 'S':
            pieceLength = 3;
            pieceHeight = 2;
            
            pieceToAdd = new char*[pieceLength]; 

            for(int i=0; i<pieceLength; i++)
            {
                    pieceToAdd[i] = new char[pieceHeight];
            }

            for(int i=0; i<pieceLength; i++)
            {
                for(int j=0; j<pieceHeight; j++)
                {
                    pieceToAdd[i][j] = S[i][j];
                }
            
            }
            break;

        case 'L':
            pieceLength = 2;
            pieceHeight = 3;
           
            pieceToAdd = new char*[pieceLength]; 

            for(int i=0; i<pieceLength; i++)
            {
                    pieceToAdd[i] = new char[pieceHeight];
            }

            for(int i=0; i<pieceLength; i++)
            {
                for(int j=0; j<pieceHeight; j++)
                {
                    pieceToAdd[i][j] = L[i][j];
                }
            
            }

            break;

        case 'J':
            pieceLength = 2;
            pieceHeight = 3;
            
            pieceToAdd = new char*[pieceLength]; 

            for(int i=0; i<pieceLength; i++)
            {
                    pieceToAdd[i] = new char[pieceHeight];
            }

            for(int i=0; i<pieceLength; i++)
            {
                for(int j=0; j<pieceHeight; j++)
                {
                    pieceToAdd[i][j] = J[i][j];
                }
            
            }
            break;

        default:
            pieceLength = 0;
            pieceHeight = 0;
            break;
    }
    
    if(pieceLength > width or x >= width)
    {
        cerr << "piece " << letter << " is too big for the board" << endl;
        for(int i=0; i<pieceLength; i++)
        {
            delete[] pieceToAdd[i];
        }

        delete[] pieceToAdd;
        exit(1);
    }   
 
    //call rotation function if necessary
    if(rotation !=0 and rotation !=360)
    { 
        rotate(pieceToAdd, rotation, pieceLength, pieceHeight); 
    }
   
    //find the maximum height of pieces on the board in order to find where to place the next piece
    maxHeight = heights[x];

    for(int i=x+1; i<pieceLength+x; i++)
    {
        if(heights[i] > maxHeight)
        {
            maxHeight = heights[i];
        } 
    } 
    
    //find where to place the next piece
    int newHeight = 0;

    if(maxHeight > 0)
    {
        //"drop" the piece until there is a collision
        //once a collision occurs, set the height for the new piece to be placed one row above the collision
        for(int i=maxHeight-1; i>=(maxHeight-pieceHeight-1) and newHeight==0; i--) 
        {
            for(int j=x; j<pieceLength+x and newHeight==0; j++)
            {
                for(int k=i; k<maxHeight and newHeight==0; k++) 
                {
                    if(heights[j] > k and heights[j] > 0 and pieceToAdd[j-x][k-i] != ' ' and data[j][k] != ' ') 
                    {
                        newHeight = i+1;
                    }
                    
                 }   
             }    
         }
    }

    //creates a temporary array of data of increased size to allocate space for the new piece to add 
    for(int i=x; i<pieceLength+x; i++)
    {
        char* tempData = new char[newHeight+pieceHeight];
        for(int j=0; j<newHeight+pieceHeight; j++)
        {
            if(j< heights[i]) 
            {
                //copy old data
                 tempData[j] = data[i][j];
            }
            else if(j<newHeight and j>=heights[i])
            {
                //pad with spaces
                tempData[j] = ' ';
            }
            else
            {
                //add new piece
                tempData[j] = pieceToAdd[i-x][j-newHeight];
            }
       } 
       
      delete[] data[i]; 
      data[i] = tempData;
    }

    //updates heights array by adding height of new piece
    for(int i=x; i<pieceLength+x; i++)
    {
        heights[i] = newHeight+pieceHeight;
    }
    
    
    //removes excess spaces at top of array
    for(int i=0; i<width; i++)
    {
        while(heights[i] > 0 and data[i][heights[i]-1] == ' ')
        {
            char* temp = new char[heights[i]-1];
            for(int j=0; j<heights[i]-1; j++)
            {
                temp[j] = data[i][j];
            }
           
            heights[i]--;
 
            delete[] data[i];
            data[i] = temp;
        }

    }

    for(int i=0; i<pieceLength; i++)
    {
        delete[] pieceToAdd[i];
    }
    
    delete[] pieceToAdd;
    pieceToAdd = NULL;
}


int Tetris::get_max_height() const
{
    int maxHeight = heights[0];

    for(int i=1; i<width;i++)
    {
        if(heights[i] > maxHeight)
        {
            maxHeight = heights[i];
        }
    }
    
    return maxHeight;
}

int Tetris::count_squares() const
{
    int numOfSquares = 0;

    for(int i=0; i<width; i++)
    {
        for(int j=0; j<heights[i]; j++)
        {
           if(data[i][j] != ' ')
           {
                numOfSquares++;
           } 
        }
    }
    
    return numOfSquares;
}

int Tetris::remove_full_rows()
{
    bool full = true;
    int numRowsFull = 0;

    //finds the lowest height as no rows above that could be full
    int  lowestHeight = heights[0];
    for(int i=1; i<width; i++)
    {
        if(heights[i] < lowestHeight)
        {
            lowestHeight = heights[i];
        }
    }

    //finds all the rows that are full
    bool isFull[lowestHeight];  
    
    for(int row=0; row<lowestHeight; row++)
    {
        for(int column=0; column<width and full; column++)
        {
            if(data[column][row] == ' ')
            {
                full = false; 
            }
        }

        isFull[row] = full;
        full = true;
     }

    for(int i=0; i< lowestHeight; i++)
    {
        if(isFull[i])
        {
            numRowsFull++;
        }
    }
   
    int offset; 
    for(int column=0; column<width; column++)
    {   
        offset = 0;
        //create new temp array and copy everything in the data column but the item in the row that was full
        char* temp = new char[heights[column]-numRowsFull];
        for(int i=0; i<heights[column]; i++)
        {
            if(i>=lowestHeight or !isFull[i])
            {
                temp[i-offset] = data[column][i];
            }
            else
            {
                offset++;
            }
        }

        delete[] data[column];
        data[column] = temp;
    }
           
    //update heights array
    for(int i=0; i<width; i++)
    {
        if(heights[i]-numRowsFull >= 0)
        {
            heights[i]-=numRowsFull;
        }
    }            
        
    //removes excess spaces at top of array
   for(int i=0; i<width; i++)
   {
        while(heights[i] > 0 and data[i][heights[i]-1] == ' ')
        {
            char* temp = new char[heights[i]-1];
            for(int j=0; j<heights[i]-1; j++)
            {
                temp[j] = data[i][j];
            }
                   
             heights[i]--; 
             delete[] data[i];
             data[i] = temp;
        }

   } 
        
   return numRowsFull;
}

void Tetris::rotate(char**& pieceToRotate, int rotation, int &pieceToRotateLength, int &pieceToRotateHeight)
{
       int w,h; 

       if(rotation==180)
       {

            w = pieceToRotateLength;
            h = pieceToRotateHeight;

            //create temporary array
            char** temp = new char*[w];
            
            //loop through array to point to new array creating a 2d array
            for(int i=0; i<w; i++)
            {
                temp[i] = new char[h];
            }
        

            for(int i=0; i<w; i++)
            {
                for(int j=0; j<h; j++)
                {
                    temp[i][j] = pieceToRotate[w-1-i][(h-j-1)];
                }
            }
           
 
            for(int i=0; i<w; i++)
            {
                 delete[] pieceToRotate[i];
            }    
    
            delete[] pieceToRotate;
            
            pieceToRotate = temp;

       }
       else if(rotation%90 ==0)
       {
            w = pieceToRotateHeight;
            h = pieceToRotateLength;
            pieceToRotateLength = w;
            pieceToRotateHeight = h;       

            //create temporary array
            char** temp = new char*[w];
            
            //loop through array to point to new array creating a 2d array
            for(int i=0; i<w; i++)
            {
                temp[i] = new char[h];
            }
           
            if(rotation == 90) 
            {
                for(int i=0; i<w; i++)
                {
                    for(int j=0; j<h; j++)
                    {
                        temp[i][j] = pieceToRotate[(h-1)-j][i];
                    }
                }
            }
            else if(rotation == 270)
            {
                for(int i=0; i<w; i++)
                {
                    for(int j=0; j<h; j++)
                    {
                        temp[i][j] = pieceToRotate[j][w-1-i];
                    }
                }
            }

             for(int i=0; i<h; i++)
             {
                delete[] pieceToRotate[i];
             }    
    
             delete[] pieceToRotate;
    
            pieceToRotate = temp;
            
    }      
    else
    {
        cerr << "can not turn peice " << rotation << " degrees" << endl;
        exit(1);
    }
  
}

void Tetris::add_left_column()
{
    //copies all heights to a new heights array and allocates space for a new left column
    width+=1;
    int* tempHeights = new int[width];
    for(int i=0; i<width-1; i++)
    {
        tempHeights[i+1] = heights[i];
    }
    
    tempHeights[0] = 0;    
    swap(tempHeights,heights);

    char** temp  = new char*[width];
    for(int i=0; i<width; i++)
    {
        temp[i] = new char[heights[i]];
    }

    for(int i=0; i<width-1; i++)
    {
        for(int j=0; j<heights[i+1]; j++)
        {
            temp[i+1][j] = data[i][j];
        }
    }

    swap(temp,data);
    for(int i=0; i<width-1; i++)
    {
        delete[] temp[i];
    }
    delete[] temp;
    delete[] tempHeights;
}

void Tetris::add_right_column()
{
    //copies all heights to a new heights and allocates space for a new column
    width+=1;
    int* tempHeights = new int[width];
    for(int i=0; i<width-1; i++)
    {
        tempHeights[i] = heights[i];
    }
    
    tempHeights[width-1] = 0;    
    swap(tempHeights,heights);

    char** temp  = new char*[width];
    for(int i=0; i<width; i++)
    {
        temp[i] = new char[heights[i]];
    }

    for(int i=0; i<width-1; i++)
    {
        for(int j=0; j<heights[i]; j++)
        {
            temp[i][j] = data[i][j];
        }
    }

    delete[] tempHeights;

    swap(temp,data);
    
    for(int i=0; i<width-1; i++)
    {
        delete[] temp[i];
    }
    delete[] temp;
}

void Tetris::remove_left_column()
{
    if((width-1) <= 0)
    {
        cerr << "cannot remove column. Board too small" << endl;
        exit(1);
    }

    //copies all heights from the heights array excpet for the leftmost column
    int* tempHeights = new int[width-1];  

    for(int i=0; i<width-1; i++)
    {
       tempHeights[i] = heights[i+1]; 
    }
    
    width--;
    swap(heights, tempHeights);
    
    char** temp = new char*[width];
    for(int i=0; i<width; i++)
    {
        temp[i] = new char[heights[i]];
    }
    
    for(int i=0; i<width; i++)
    {
        for(int j=0; j<heights[i]; j++)
        {
            temp[i][j] = data[i+1][j];
        }
    }
    
    delete[] tempHeights;

    swap(temp,data);
    
    for(int i=0; i<width+1; i++)
    {
        delete[] temp[i];
    }
    delete[] temp;
}

void Tetris::remove_right_column()
{
    if((width-1) <= 0)
    {
        cerr << "cannot remove column. Board too small" << endl;
        exit(1);
    }

    //copies all heights from the old heights array except the rightmost column
    int* tempHeights = new int[width-1];  

    for(int i=0; i<width-1; i++)
    {
       tempHeights[i] = heights[i]; 
    }
    
    width--;
    swap(heights, tempHeights);
    
    char** temp = new char*[width];
    for(int i=0; i<width; i++)
    {
        temp[i] = new char[heights[i]];
    }
    

    for(int i=0; i<width; i++)
    {
        for(int j=0; j<heights[i]; j++)
        {
            temp[i][j] = data[i][j];
        }
    }
    
    delete[] tempHeights;

    swap(temp,data);
    
    for(int i=0; i<width+1; i++)
    {
        delete[] temp[i];
    }
    delete[] temp;
}

void Tetris::destroy()
{
    delete[] heights;
    for(int i=0; i<width; i++)
    {
        delete[] data[i];
    }
    
    delete[] data;
}


