#include "../include/typedef.h"
#include "../include/utils.h"
#include "../include/course.h"
#include "../include/pgstudent.h"
#include <cstdlib>
#include <fstream>
using namespace std;

PGStudent::PGStudent(std::vector<std::string> data, 
                     int elective_courses_count) 
                    : Student(data, elective_courses_count) 
{
}

void PGStudent::study(Course& c) {
    if (rand()%101 < 20) {
        // student is slacking off the course.
        Utils::log(_id, " is slacking off ", c.getName());
        return;  
    } 
    
    // the student handled the workload during the semester, now
    // he should try his luck at the exam.
    takeExam(c);
}
