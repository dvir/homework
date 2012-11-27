#include "../include/typedef.h"
#include "../include/utils.h"
#include "../include/student.h"
#include "../include/course.h"
#include "../include/cscourse.h"
#include <vector>
#include <string>
#include <fstream>
using namespace std;

CSCourse::CSCourse(std::vector<std::string> data) : Course(data) {

}

void CSCourse::reg(Student& s) {
    _students.push_back(&s);
    
    Utils::log(s.getId(), " is taking ", _name, " from CS");
}
