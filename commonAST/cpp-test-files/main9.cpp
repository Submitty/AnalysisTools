#include <cstdlib>
#include <cmath>
#include <vector>
#include <iostream>
#include <set>

#include "/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw9/image.h"
#include "/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw9/priority_queue.h"


using namespace std;

// ===================================================================================================

// distance field method functions
double NaiveDistanceFieldMethod(Image<Color> &input, Image<DistancePixel> &distance_image);
double ImprovedDistanceFieldMethod(Image<Color> &input, Image<DistancePixel> &distance_image);
double FastMarchingMethod(Image<Color> &input, Image<DistancePixel> &distance_image);
void propogateQueue(DistancePixel_PriorityQueue& p, Image<Color> &input, Image<DistancePixel> &distance_image, double &max);

// visualization style helper functions
Color Rainbow(double distance, double max_distance);
Color GreyBands(double distance, double max_distance, int num_bands);

// ===================================================================================================

int main(int argc, char* argv[]) {

    if (argc != 5) {
        std::cerr << "Usage: " << argv[0] << " input.ppm output.ppm distance_field_method visualization_style" << std::endl;
        exit(1);
    }

    // open the input image
    Image<Color> input;
    if (!input.Load(argv[1])) {
        std::cerr << "ERROR: Cannot open input file: " << argv[1] << std::endl;
        exit(1);
    }

    // a place to write the distance values
    Image<DistancePixel> distance_image;
    distance_image.Allocate(input.Width(),input.Height());

    // calculate the distance field (each function returns the maximum distance value)
    double max_distance = 0;
    if (std::string(argv[3]) == std::string("naive_method")) {
        max_distance = NaiveDistanceFieldMethod(input,distance_image);
    } else if (std::string(argv[3]) == std::string("improved_method")) {
        max_distance = ImprovedDistanceFieldMethod(input,distance_image);
    } else if (std::string(argv[3]) == std::string("pq_with_map")) {
        max_distance = FastMarchingMethod(input,distance_image);
    } else if (std::string(argv[3]) == std::string("pq_with_hash_table")) {
        // EXTRA CREDIT: implement FastMarchingMethod with a hash table
    } else {
        std::cerr << "ERROR: Unknown distance field method: " << argv[3] << std::endl;
        exit(1);
    }

    // convert distance values to a visualization
    Image<Color> output;
    output.Allocate(input.Width(),input.Height());
    for (int i = 0; i < input.Width(); i++) {
        for (int j = 0; j < input.Height(); j++) {
            double v = distance_image.GetPixel(i,j).getValue();
            if (std::string(argv[4]) == std::string("greyscale")) {
                output.SetPixel(i,j,GreyBands(v,max_distance*1.01,1));
            } else if (std::string(argv[4]) == std::string("grey_bands")) {
                output.SetPixel(i,j,GreyBands(v,max_distance,4));
            } else if (std::string(argv[4]) == std::string("rainbow")) {
                output.SetPixel(i,j,Rainbow(v,max_distance));
            } else {
                // EXTRA CREDIT: create other visualizations 
                std::cerr << "ERROR: Unknown visualization style" << std::endl;
                exit(0);
            }
        }
    }
    // save output
    if (!output.Save(argv[2])) {
        std::cerr << "ERROR: Cannot save to output file: " << argv[2] << std::endl;
        exit(1);
    }

    return 0;
}

// ===================================================================================================

double NaiveDistanceFieldMethod(Image<Color> &input, Image<DistancePixel> &distance_image) {
    int w = input.Width();
    int h = input.Height();
    // return the maximum distance value
    double answer = 0;
    // loop over the pixels in the input image
    for (int i = 0; i < w; i++)  {
        for (int j = 0; j < h; j++) {
            double closest = -1;      
            // loop over all other pixels in the input image
            for (int i2 = 0; i2 < w; i2++)  {
                for (int j2 = 0; j2 < h; j2++) {
                    const Color& c = input.GetPixel(i2,j2);      
                    // skip all pixels that are not black
                    if (!c.isBlack()) continue;
                    // calculate the distance between the two pixels
                    double distance = sqrt((i-i2)*(i-i2) + (j-j2)*(j-j2));
                    // store the closest distance to a black pixel
                    if (closest < 0 || distance < closest) {
                        closest = distance;
                    }
                }
            }
            assert (closest >= 0);
            answer = std::max(answer,closest);
            // save the data to the distance image
            DistancePixel& p = distance_image.GetPixel(i,j);
            p.setValue(closest);
        }
    }
    return answer;
}


double ImprovedDistanceFieldMethod(Image<Color> &input, Image<DistancePixel> &distance_image) {

    //
    // IMPLEMENT THIS FUNCTION
    //
    // a small improvement on the NaiveDistanceFieldMethod
    //
    int w = input.Width();
    int h = input.Height();

    vector<pair<int,int> > blackPoints; 

    for(int i = 0; i < w; i++)
    {
        for(int j = 0; j < h; j++)
        {
            const Color& c = input.GetPixel(i,j); 
            if(c.isBlack())
            {
                blackPoints.push_back(make_pair(i,j)); 
            }
        }
    }

    // return the maximum distance value
    double answer = 0;
    // loop over the pixels in the input image
    for (int i = 0; i < w; i++)  {
        for (int j = 0; j < h; j++) {
            double closest = -1;      
            // loop over all other pixels in the input image
            for (int pixel = 0; pixel < blackPoints.size(); pixel++)  {
                int i2 = blackPoints[pixel].first;
                int j2 = blackPoints[pixel].second;

                // calculate the distance between the two pixels
                double distance = sqrt((i-i2)*(i-i2) + (j-j2)*(j-j2));
                // store the closest distance to a black pixel
                if (closest < 0 || distance < closest) {
                    closest = distance;
                }
            }
            assert (closest >= 0);
            answer = std::max(answer,closest);
            // save the data to the distance image
            DistancePixel& p = distance_image.GetPixel(i,j);
            p.setValue(closest);
        }
    }
    return answer;
}

//improved algorithm using a priority queue
double FastMarchingMethod(Image<Color> &input, Image<DistancePixel> &distance_image) {

    
    int w = input.Width();
    int h = input.Height();
    
    vector<DistancePixel*> blackPixels; 
    //put things in a set rather than a vector to avoid having to check for duplicates
    set<DistancePixel*> otherPixelsSet;

    //get all black pixels from the image 
    //and initialize their distance image value to 0
    //initialize everything else to a very large number
    for(int i = 0; i < w; i++)
    {
        for(int j = 0; j < h; j++)
        {
            const Color& c = input.GetPixel(i,j); 
            DistancePixel& p = distance_image.GetPixel(i,j);
            p.setX(i);
            p.setY(j); 
            if(c.isBlack())
            {
                p.setValue(0);
                DistancePixel* temp = &p;
                blackPixels.push_back(temp);
            }
        }
    }

    //for each black pixel, loop over their 8 neighbors
    for(int i = 0; i < blackPixels.size(); i++)
    {
        int x = blackPixels[i]->getX();
        int y = blackPixels[i]->getY();

        for(int x2 = x-1; x2 <= x+1; x2++) 
        {
            for(int y2 = y-1; y2 <= y+1; y2++)
            {
                //check bounds
                if(y2 < 0 || x2 < 0 || y2 >= h || x2 >= w) 
                {
                    continue;
                }

                //skip the black pixels
                if((x2 == x && y2 == y) || input.GetPixel(x2,y2).isBlack())
                {
                    continue;
                }

                //calculate the distance
                double dist = sqrt((y-y2)*(y-y2) + (x-x2)*(x-x2));
                
                if(distance_image.GetPixel(x2,y2).getValue() > dist)
                {
                    //set distance to actual value
                    DistancePixel* temp = &distance_image.GetPixel(x2,y2);
                    temp->setValue(dist); 

                    //push temp to otherPixels set
                    otherPixelsSet.insert(temp);
                }
            }
        }
    }

    //convert set to vector
    vector<DistancePixel*> otherPixels( otherPixelsSet.begin(), otherPixelsSet.end());

    double max = 0;

    //create the queue
    DistancePixel_PriorityQueue pQueue = DistancePixel_PriorityQueue(otherPixels); 
    //call helper function to calculate the distances of the rest of the image
    propogateQueue(pQueue, input, distance_image, max);

    return max;
}

//function to propogate the priority queue for all pixels in the input image
void propogateQueue(DistancePixel_PriorityQueue& p, Image<Color> &input, Image<DistancePixel> &distance_image, double &max)
{
    int w = input.Width();
    int h = input.Height();

    const DistancePixel* min;
    while(!p.empty())
    {
        //pop the top pixel from the queue 
        min = p.top();
        //set the value we are about to pop off as "final" (see image.h for more on this)
        p.pop();

        //update max 
        if(min->getValue() > max)
        {
            max = min->getValue();
        }

        int x = min->getX();
        int y = min->getY(); 
        //for each of the 8 neighbors of the popped off pixel
        for(int x2 = x-1; x2 <= x+1; x2++) 
        {
            for(int y2 = y-1; y2 <= y+1; y2++)
            {
                //check bounds  
                if(y2 < 0 || x2 < 0 || y2 >= h || x2 >= w)
                {
                    continue;
                }

                //skip the black pixels and the pixels which have already been popped off the queue
                if((x2 == x && y2 == y) || input.GetPixel(x2,y2).isBlack() 
                    || distance_image.GetPixel(x2,y2).getFinal())
                {
                    continue;
                }

                //calculate distance relative to known value
                double dist = min->getValue() + sqrt(((y-y2)*(y-y2)) + ((x-x2)*(x-x2)));

                if(distance_image.GetPixel(x2,y2).getValue() > dist) 
                {
                    DistancePixel* temp = &distance_image.GetPixel(x2,y2);
                    temp->setValue(dist); 

                    //if its not already in the heap, add it
                    if (!p.in_heap(temp))
                    {
                        p.push(temp);
                    }
                    else
                    {
                        p.update_position(temp);
                    }
                }
            }
        }
    }
}

// ===================================================================================================

Color Rainbow(double distance, double max_distance) {
    Color answer;
    if (distance < 0.001) {
        // black
        answer.r = 0; answer.g = 0; answer.b = 0;
    } else if (distance < 0.2*max_distance) {
        // blue -> cyan
        double tmp = distance * 5.0 / max_distance;
        answer.r = 0;
        answer.g = tmp*255;
        answer.b = 255;
    } else if (distance < 0.4*max_distance) {
        // cyan -> green
        double tmp = (distance-0.2*max_distance) * 5.0 / max_distance;
        answer.r = 0;
        answer.g = 255;
        answer.b = (1-tmp*tmp)*255;
    } else if (distance < 0.6*max_distance) {
        // green -> yellow
        double tmp = (distance-0.4*max_distance) * 5.0 / max_distance;
        answer.r = sqrt(tmp)*255;
        answer.g = 255;
        answer.b = 0;
    } else if (distance < 0.8*max_distance) {
        // yellow -> red
        double tmp = (distance-0.6*max_distance) * 5.0 / max_distance;
        answer.r = 255;
        answer.g = (1-tmp*tmp)*255;
        answer.b = 0;
    } else if (distance < max_distance) {
        // red -> white
        double tmp = (distance-0.8*max_distance) * 5.0 / max_distance;
        answer.r = 255;
        answer.g = tmp*255;
        answer.b = tmp*255;
    } else {
        // white
        answer.r = answer.g = answer.b = 255;
    }  
    return answer;
}

Color GreyBands(double distance, double max_value, int num_bands) {
    Color answer;
    if (distance < 0.001) {
        // red
        answer.r = 255; answer.g = 0; answer.b = 0;
    } else {
        // shades of grey
        answer.r = answer.g = answer.b = int(num_bands*256*distance/double(max_value)) % 256;
    }  
    return answer;
}

// ===================================================================================================
