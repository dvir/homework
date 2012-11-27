#include "../include/typedef.h"
#include "../include/utils.h"
#include "../include/student.h"
#include "../include/electivecourse.h"
#include <fstream>
using namespace std;

ElectiveCourse::ElectiveCourse(std::vector<std::string> data) : Course(data) {

}

void ElectiveCourse::reg(Student& s) {
    _students.push_back(&s);
    
    Utils::log(s.getId(), " is taking ", _name, " from ELECTIVE");
}
