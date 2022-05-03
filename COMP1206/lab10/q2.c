typedef int* NEWTYPE;

NEWTYPE ARRAY(int N);
int STORE(NEWTYPE as, int N, int row, int col, int val);
int FETCH(NEWTYPE as, int N, int row, int col);

NEWTYPE ARRAY(int N)
{
	return malloc(sizeof(int) * (N * N));
}

int STORE(NEWTYPE as, int N, int row, int col, int val)
{
	if ((row % 2 == col % 2) && (row < N && col < N) && (row >= 0 && col >= 0))
	{
		as[(N * row) + col] = val;
		return 1;
	}

	return -1;
}

int FETCH(NEWTYPE as, int N, int row, int col)
{
	if ((row % 2 == col % 2) && (row < N && col < N) && (row >= 0 && col >= 0))
	{
		return as[(N * row) + col];
	}

	return -1;
}