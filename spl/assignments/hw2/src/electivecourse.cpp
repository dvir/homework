#include "../include/typedef.h"
#include "../include/student.h"
#include "../include/electivecourse.h"
#include <iostream>
using namespace std;

ElectiveCourse::ElectiveCourse(std::vector<std::string> data) : Course(data) {

}

void ElectiveCourse::reg(Student& s) {
    _students.push_back(&s);
    std::cout << s.getId() << " is taking " << _name << " from ELECTIVE" << std::endl;
}
