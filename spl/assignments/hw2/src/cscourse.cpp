#include "../include/typedef.h"
#include "../include/student.h"
#include "../include/course.h"
#include "../include/cscourse.h"
#include <vector>
#include <string>
#include <iostream>
using namespace std;

CSCourse::CSCourse(std::vector<std::string> data) : Course(data) {

}

void CSCourse::reg(Student& s) {
    _students.push_back(&s);
    std::cout << s.getId() << " is taking " << _name << " from CS" << endl;
}
