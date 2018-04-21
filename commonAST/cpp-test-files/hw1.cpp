#include <iostream>
#include <fstream>
#include <vector>
#include <cstdlib>

using namespace std;


/*Creates a copy of the original image as to not change it then steps through the image rows column by column and checks if the pixel directly below, above or to the right or left of it is a foreground pixel. If so, 
the pixel on the copy is changed to the foreground pixel. */
vector<string> dilate(const vector<string> & buffer, const char foreground, int xSize =1, int ySize =1)
{
	vector<string> output(buffer);
	for(int row=0; row<output.size(); row++)
	{
		for(int column=0; column<output[row].size(); column++)
		{
			if(
	((column+xSize) < (output[row].size()) 
		and buffer[row][column+xSize] == foreground) 
	|| ((row+ySize) < (output.size()) 
		and buffer[row+ySize][column] == foreground) 
	|| ((row-ySize) >= 0  
		and buffer[row-ySize][column] == foreground) 
	|| ((column-xSize) >=0 
		and buffer[row][column-xSize] == foreground))
			{
				output[row][column] = foreground;
			}

		}
	}

	return output;
}

/*Creates a copy of the original image as to not change it then steps through the image rows column by column and checks if the pixel directly below, above or to the right or left of it is a background pixel. If so, 
the pixel on the copy is changed to the background pixel. */
vector<string> erode(const vector<string> & buffer, const char background, int xSize=1, int ySize=1)
{
	vector<string> output(buffer);
	for(int row=0; row<output.size(); row++)
	{
		for(int column=0; column<output[row].size(); column++)
		{
			if(((column+xSize) < (output[row].size()) and buffer[row][column+xSize] == background) || ((row+ySize) < (output.size()) and buffer[row+ySize][column] == background) || ((row-ySize) >= 0  and buffer[row-ySize][column] == background) || ((column-xSize) >=0 and buffer[row][column-xSize] == background))
			{
				output[row][column] = background;
			}

		}
	}

	return output;
}

//Creates a copy of the original image as to not change it then steps through the image rows column by column and checks if the pixel is the char to replace. If it is, it is replaced with the replacement character
vector<string> replace(vector<string> & buffer, const char charToReplace, const char replacementChar)
{
	vector<string> output(buffer);
	for(int row=0; row<output.size(); row++)
	{
		for(int column=0; column<output[row].size(); column++)
		{
			if(buffer[row][column] == charToReplace)
			{
				output[row][column] = replacementChar;
			}

		}
	}

	return output;
}


//recursively fills points in a cluster. Only checks directly above, down, left and right. 
vector<string> floodfill(vector<string> & buffer, const int xCoor, const int yCoor, const char replacementChar)
{
	char foreground = buffer[yCoor][xCoor];
	buffer[yCoor][xCoor] = replacementChar;
	//if none touching need to be changed
	if (not((xCoor != (buffer[yCoor].size()-1) and buffer[yCoor][xCoor+1] == foreground) || (yCoor != (buffer.size()-1) and buffer[yCoor+1][xCoor] == foreground) || (yCoor!= 0  and buffer[yCoor-1][xCoor] == foreground) || (xCoor !=0 and buffer[yCoor][xCoor-1] == foreground)))
	{
		return buffer;
	}
	//check above
	if((yCoor!= 0  and buffer[yCoor-1][xCoor] == foreground))
	{
		floodfill(buffer, xCoor, yCoor-1,replacementChar);
	}
	//check below
	if((yCoor != (buffer.size()-1) and buffer[yCoor+1][xCoor] == foreground))
	{
		floodfill(buffer,xCoor,yCoor+1, replacementChar);
	}
	//check left
	if((xCoor !=0 and buffer[yCoor][xCoor-1] == foreground))
	{
		floodfill(buffer, xCoor-1, yCoor, replacementChar);
	}
	//check right
	if((xCoor != (buffer[yCoor].size()-1) and buffer[yCoor][xCoor+1] == foreground))
	{
		floodfill(buffer, xCoor+1, yCoor, replacementChar);
	}

	return buffer;
}



int main(int argc, char* argv[])
{
	//check number of arguments given and exit if not enough
	if (argc < 5)
	{
		cerr << "More arguments required" << endl;
		return 1;
	}


	string inputFileName(argv[1]);
	string outputFileName(argv[2]);
	string transform(argv[3]);

	char foregroundChar;
	char backgroundChar;
	int x;
	int y;
	char toReplace;
	char replacement;

	if (transform[0] != 'f' and transform[0] != 'r')
	{
		foregroundChar = argv[4][0];
		if(argc > 5)
		{
			backgroundChar = argv[5][0];
		}
	}
	else if(argc > 6 and transform[0] != 'r')
	{
		y = atoi(argv[4]);
		x = atoi(argv[5]);
		replacement = argv[6][0];
	}
	else if(argc >5)
	{
		toReplace = argv[4][0];
		replacement = argv[5][0];
	}


	ifstream inputFile(inputFileName.c_str());
	ofstream outputFile(outputFileName.c_str());
	vector <string> imageBuffer;
	vector <string> outputImage;

	//check if input file opens correctly and exit if not
	if(!inputFile.good())
	{
		cerr << "Can't open" << inputFileName << "to read " << endl;
		return 1;
	}

	//read image into imageBuffer vector
	string line;
	while(getline(inputFile,line))
	{
		imageBuffer.push_back(line);
	}

	//checks the first letter of the transform. It is not necessary to check the entire word as the each transform has a different first letter
	if (transform[0] == 'd')
	{
		outputImage = dilate(imageBuffer, foregroundChar);
	}
	else if(transform[0] == 'e')
	{
		outputImage = erode(imageBuffer, backgroundChar);
	}
	else if(transform[0] == 'r')
	{
		outputImage = replace(imageBuffer, toReplace, replacement);
	}
	else if(transform[0] == 'f')
	{
		outputImage = floodfill(imageBuffer, x, y, replacement);
	}
	else
	{
		cerr << "transformation not recognized" << endl;
	}


	if(outputImage.size() > 0){

		for(int i=0; i<outputImage.size(); i++)
		{
			outputFile << outputImage[i] << endl;
		}	
	}

	return 0;
}
