#include "../include/typedef.h"
#include "../include/utils.h"
#include "../include/course.h"
#include "../include/csstudent.h"
#include <cstdlib>
#include <fstream>
using namespace std;

CSStudent::CSStudent(std::vector<std::string> data, 
                     int elective_courses_count) 
                    : Student(data, elective_courses_count) 
{
}

void CSStudent::study(Course& c) {
    if (rand()%101 < 25) {
        // student didn't handle the workload and quit the course.
        Utils::log(_id, " quits course ", c.getName());
        return;  
    } 
    
    // the student handled the workload during the semester, now
    // he should try his luck at the exam.
    takeExam(c);
}
