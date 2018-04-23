#include <vector>
#include <iostream>

using namespace std;

vector< vector<int> > make_power_matrix(int num_rows, int num_columns)
{
    vector< vector<int> > power_matrix;
    for(int i=0; i< num_rows; i++)
    {
        vector<int> v2;
        for(int j=0; j<num_columns; j++)
        {
           int power = i;
           if(j!=0)
           {
                for(int k=1; k<j; k++)
                {
                    power *=i;
                }
            }
            else
            {
                power = 1;
            }
            v2.push_back(power);
        }
        power_matrix.push_back(v2);
    }
    return power_matrix;
}

int main(int argc, char* argv[])
{
    vector< vector<int> > v = make_power_matrix(5,7);
    for(int i=0; i<5; i++)
    {
        for(int j=0; j<7; j++)
        {
            cout << v[i][j] << " ";
        }   
        cout << endl;
    }

    if(string(argv[1]) == "blue")
    {
        cout << "YEP" << endl;
    }    

    return 0;
}
