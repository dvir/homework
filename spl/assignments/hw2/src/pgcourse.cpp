#include "../include/typedef.h"
#include "../include/utils.h"
#include "../include/student.h"
#include "../include/pgcourse.h"
#include <fstream>
using namespace std;

PGCourse::PGCourse(std::vector<std::string> data) : Course(data) {

}

void PGCourse::reg(Student& s) {
    _students.push_back(&s);

    Utils::log(s.getId(), " is taking ", _name, " from PG");
}
