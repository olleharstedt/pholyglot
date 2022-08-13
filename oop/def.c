#define REPLACE(s) moo_##s

void REPLACE(me)()
{
}

int main()
{
    moo_me();
    return 0;
}
