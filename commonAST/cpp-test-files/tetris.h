class Tetris{

public:
	//CONSTRUCTOR
    Tetris(int w);
    
    void add_piece(char letter, int rotation, int x);
    int get_width() const { return width; }
    int get_max_height() const;
    int count_squares() const;
    void print() const;
    int remove_full_rows();    
    void rotate(char**& pieceToRotate, int rotation, int &pieceToRotateLength, int &pieceToRotateHeight);
    void add_left_column();
    void add_right_column();
    void remove_left_column();
    void remove_right_column();
    void destroy();

private:
	int width;
	int* heights;
	char** data;

    const static char I[1][4];
    const static char O[2][2]; 
    const static char T[3][2];
    const static char Z[3][2];
    const static char S[3][2];
    const static char L[2][3];
    const static char J[2][3];
};
