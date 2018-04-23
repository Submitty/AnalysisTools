vector<string> mystery(const vector<string> &input) 
{
    if (input.size() == 1) { return input; }

    vector<string> output;

    for (int i = 0; i < input.size(); i++) 
    {
        vector<string> helper_input;
        for (int j = 0; j < input.size(); j++)
        {
            if (i == j) continue;
            
            helper_input.push_back(input[j]);
        }

        vector<string> helper_output = mystery(helper_input);

        for (int k = 0; k < helper_output.size(); k++) 
        {
            output.push_back(input[i]+", "+helper_output[k]);
        }
    }

    return output;

}

