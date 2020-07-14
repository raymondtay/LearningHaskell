#include <iostream>
#include <vector>
#include <type_traits>
using namespace std;

template<typename T>
using Any = typename decay<T>::type;

template<typename T>
class List {
private:
  vector<T> repr;
protected:
  vector<T>& get_repr() { return repr; }
};

template<typename T>
struct Cons : public List<T> {
  Cons& operator()(T ele) { 
    cout << "Pushing : " << ele << endl;
    this->get_repr().push_back(ele);
    return *this;
  }
};

//class Nil : public List<Any> {};

int main() {
  Cons<int> is;
  is(4)(5)(6);
}


