# oops-lang

How to compile and run OOPs:

    1. $ make
    2. $ ./oops.native <name_of_your_file>.oops 
    3. $ ./a.out

TO-DO:
    Semantics: check method calls and arrays 
    Irgen: add support for class object types, array declarations, and initializations

Testing the Parser:

    1. $ ocamlbuild test1.native
    2. $ ./test1.native
    3. Enter the sample input
    4. Ctrl D 

    Sample Input:

    public class Test{
        int a;
        int test_func(int b){
            a = b;
        }
    }

Testing the Semantics:

    1. $ocamlbuild test2.native
    2. $./test2.native
    3. Enter the sample intput
    4. Ctrl D

    Sample Input:

    // this causes a duplicate class name error

    public class Test{
        int test(){
            return 4;
        }
    }
    public class Test{
        int test(){
            return 5;
        }
    }
    int main(){
        Test t1;
        return 0;
    }

