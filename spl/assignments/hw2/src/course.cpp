#include "../include/typedef.h"
#include "../include/utils.h"
#include "../include/course.h"
#include "../include/student.h"
#include <iostream>
#include <vector>
#include <string>
#include <cstdlib>
using namespace std;

Course::Course(std::vector<std::string> data):
    _students(),
    _dept(data[0]),
    _name(data[1]),
    _semester(atoi(data[2].c_str())),
    _min_grade(atoi(data[3].c_str()))
{
}

void Course::teach() {
    for (size_t i = 0; i < _students.size(); ++i) {
        _students[i]->study(*this);
    }

    reset();
}

void Course::reset() {
    // the semester ended. clean the course registration list.
    _students.clear();
}
