#include "../include/typedef.h"
#include "../include/student.h"
#include "../include/pgcourse.h"
#include <iostream>
using namespace std;

PGCourse::PGCourse(std::vector<std::string> data) : Course(data) {

}

void PGCourse::reg(Student& s) {
    _students.push_back(&s);
    std::cout << s.getId() << " is taking " << _name << " from PG" << std::endl;
}
