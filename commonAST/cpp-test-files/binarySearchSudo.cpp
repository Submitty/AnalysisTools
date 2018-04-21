
void mergeSort(const vector<int>& v, int low, int high)
{
    if(v.size() <= 1)
    {
        return;
    } 

    int mid = (low+high)/2;
    vector<int> temp;

    int left = low;
    int right = mid;

    mergeSort(v, left, right);
    mergeSort(v, right, high); 

    while(temp.size() < high-low)
    {
        if(left < mid && right < high)
        {
            if(v[left] < v[right])
            {
                temp.push_back(v[left]);
                left++;
            }
            else
            {
                temp.push_back(v[right]);
                right++;
            }
            
        }
        else if(left < mid)
        {
            temp.push_back(v[left]);
            left++;
        } 
        else
        {
            temp.push_back(v[right]);
            right++;
        }
    } 

    v = temp;
}

