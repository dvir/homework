/**
 * Minimize Boolean Algebra Function - Quine-McCluskey algorithm implementation.
 *
 * @author: Dvir Azulay (dvir.azulay@gmail.com)
 *
 * @Example:
 *   - Compiling: g++ -g -Wall -Weffc++ -o qmc qmc.cpp -lboost_program_options
 *   - ./qmc -l 4 --sop 0 1 2 5 6 7 8 9 10 14
 *   - Output: f = cd' + b'c' + a'bd
*/

#include <vector>
#include <queue>
#include <tr1/unordered_map>
#include <iostream>
#include <string>
#include <cmath>
#include <algorithm>
#include <cstdlib>
#include <iomanip>
#include <sstream>
#include <boost/program_options.hpp>
#include <boost/foreach.hpp>
using namespace std;

class Binary {
    public:
        Binary() : _num(0) {
        };

        Binary(unsigned int num) : _num(num) {
        };

        virtual ~Binary() { };

        string getBinary() const {
            if (_num == 0) {
                return "0";
            }

            unsigned int num = _num;
            string binary;

            while (num > 0) {
                string mod = "0";
                if (num % 2 == 1) {
                    mod = "1";
                }
               
                binary.insert(0, mod);
                num = num / 2;
            }

            return binary;
        };

        virtual unsigned int at(size_t i) const {
            return (_num & (2 << i));
        };

        virtual unsigned int oneCount(string binary) const {
            unsigned int one_count = 0;
            for (size_t i = 0; i < binary.length(); i++) {
                if (binary.at(i) == '1') {
                    one_count++;
                }
            }

            return one_count;
        };

        virtual unsigned int oneCount() const {
            return oneCount(getBinary());
        };
        
        unsigned int getDecimal() const {
            return _num;
        };

    protected:
        unsigned int _num;
};

class Term : public Binary {
    public:
        Term(unsigned int num, size_t literals_count) : 
            Binary(num), 
            _dashes(), 
            _minterms(),
            _remaining_minterms(),
            _selected(false),
            _prime_implicant(false),
            _dont_care(false),
            _literals_count(literals_count)
        {
            _minterms.push_back(num);
        };

        Term(Term& first, Term& second, size_t new_dash) : 
            Binary(max(first.getDecimal(), second.getDecimal())),
            _dashes(),
            _minterms(),
            _remaining_minterms(),
            _selected(false),
            _prime_implicant(false),
            _dont_care(first.isDontCare() && second.isDontCare()),
            _literals_count(max(first.getLiteralCount(), second.getLiteralCount()))
        {
            for (vector<size_t>::iterator it = first.getMinterms().begin();
                 it < first.getMinterms().end(); ++it)
            {
                _minterms.push_back(*it);
            }
            
            for (vector<size_t>::iterator it = second.getMinterms().begin();
                 it < second.getMinterms().end(); ++it)
            {
                _minterms.push_back(*it);
            }

            for (vector<size_t>::iterator it = first.getDashes().begin();
                 it < first.getDashes().end(); ++it)
            {
                _dashes.push_back(*it);
            }

            for (vector<size_t>::iterator it = second.getDashes().begin();
                 it < second.getDashes().end(); ++it)
            {
                _dashes.push_back(*it);
            }

            _dashes.push_back(new_dash);
        };
        
        virtual ~Term() { };

        string getExpression() const {
            string expression = getBinary();
            expression.insert(0, _literals_count - expression.length(), '0');

            for (vector<size_t>::const_iterator it = _dashes.begin();
                 it < _dashes.end(); ++it)
            {
                expression.replace(expression.length() - (*it), 1, "-");
            }

            return expression;
        };

        void addDash(size_t i) {
            _dashes.push_back(i);
        };

        using Binary::oneCount;
        virtual unsigned int oneCount() const {
            return oneCount(getExpression());  
        };

        inline bool operator<(const Term& other) const {
            return (this->oneCount() < other.oneCount());
        };

        inline bool operator>(const Term& other) const {
            return (this->oneCount() > other.oneCount());
        };

        bool isSelected() const {
            return _selected;
        };

        void select() {
            _selected = true;
        };
        
        bool isPrimeImplicant() const {
            return _prime_implicant;
        };

        void primeImplicant() {
            _prime_implicant = true;
        };
        
        bool isDontCare() const {
            return _dont_care;
        };

        void dontCare() {
            _dont_care = true;
        };
        
        virtual char charAt(size_t i) const {
            if (i >= getExpression().size()) {
                return '0';
            }

            return getExpression().at(getExpression().length() - i - 1);
        };
        
        int separatingBit(const Term& other) const {
            int separating_bit = -1;
            int max_num = max(_num, other.getDecimal());
            for (size_t i = 0; max_num > 0; ++i) {
                if (charAt(i) != other.charAt(i)) {
                    if (separating_bit != -1) {
                        // we already had one bit separating between
                        // the terms! that means the terms are separated
                        // by two or more bits.
                        return -1;
                    }

                    separating_bit = i;
                }

                max_num = max_num / 2;
            }

            return separating_bit;
        };

        vector<size_t>& getMinterms() {
            return _minterms;
        };
        
        vector<size_t>& getDashes() {
            return _dashes;
        };

        string getDecimals() const {
            stringstream decimals;
            if (_minterms.size() > 0) {
                decimals << _minterms.at(0);
            }

            for (vector<size_t>::const_iterator it = _minterms.begin()+1;
                 it < _minterms.end(); ++it)
            {
                decimals << "," << *it;
            }

            return decimals.str();
        };

        size_t getLiteralCount() const {
            return _literals_count;
        };

        string getLiterals() const {
            string expression = getExpression();
            string literal;
            for (size_t i = 0; i < _literals_count; ++i) {
                char c = '0';
                if (i < expression.length()) {
                    c = expression.at(i);
                }

                if (c != '-') {
                    literal += (char)(i + 'a');
                }

                if (c == '0') {
                    literal += '\'';
                }
            }
            
            return literal;
        };

        string getDashesIndices() const {
            stringstream indices;
            if (_dashes.size() > 0) {
                indices << _dashes.at(0);
            }

            for (vector<size_t>::const_iterator it = _dashes.begin()+1;
                 it < _dashes.end(); ++it)
            {
                indices << "," << *it;
            }

            return indices.str();
        };

        void coverMinterm(size_t i) {
            vector<size_t>::iterator position = find(_remaining_minterms.begin(), _remaining_minterms.end(), i);
            if (position != _remaining_minterms.end()) {
                _remaining_minterms.erase(position);
            }
        };

        void primeImplicantCandidate() {
            _remaining_minterms = _minterms;   
        };

        vector<size_t> getRemainingMinterms() const {
            return _remaining_minterms;
        };

    private:
        vector<size_t> _dashes;
        vector<size_t> _minterms;
        vector<size_t> _remaining_minterms;
        bool _selected;
        bool _prime_implicant;
        bool _dont_care;
        size_t _literals_count;
};

std::ostream& operator<<(std::ostream &strm, const Term& term) {
    return strm << term.getLiterals();
}

struct PointerCompare {
    bool operator()(const Term* left, const Term* right) {
        return (*left) < (*right);
    }
};

struct PrimeImplicantCandidatesCompare {
    bool operator()(const Term* left, const Term* right) {
        return (left->getRemainingMinterms().size() >= right->getRemainingMinterms().size());
    }
};

typedef vector<Term*> Terms;
typedef tr1::unordered_map<size_t, Terms> ImplicantsMap;

string terms_function(const vector<Term*>& terms, bool debug=false) {
    string minimized_function;
    if (terms.size() > 0) {
        minimized_function.append(terms.at(0)->getLiterals());

        if (debug) {
            minimized_function.append(" (");
            minimized_function.append(terms.at(0)->getDecimals());
            minimized_function.append(")");
        }
    }
    
    for (Terms::const_iterator it = terms.begin()+1; 
        it < terms.end(); ++it) 
    {
        minimized_function.append(" + ");
        minimized_function.append((*it)->getLiterals());

        if (debug) {
            minimized_function.append(" (");
            minimized_function.append((*it)->getDecimals());
            minimized_function.append(")");
        }
    }

    return minimized_function;
}

ostream& operator<<(ostream& output, const vector<string>& list) {
    if (list.size() > 0) {
        output << list.at(0);
    }

    for (vector<string>::const_iterator it = list.begin()+1;
         it != list.end(); ++it)
    {
        output << " " << *it;
    }

    return output;
}

ostream& operator<<(ostream& output, const Terms& terms) {
    if (terms.size() > 0) {
        output << terms.at(0)->getLiterals() << " (" << terms.at(0)->getDecimals() << ")";
    }

    for (Terms::const_iterator it = terms.begin()+1;
         it != terms.end(); ++it)
    {
        output << (*it)->getLiterals() << " (" << (*it)->getDecimals() << ")";
    }

    return output;
}

int main(int argc, char** argv) {
    namespace po = boost::program_options;

    int literals_count;
    bool debug;

    po::options_description desc("Allowed options");
    desc.add_options()
        ("help", "produce help message")
        ("sop", po::value< vector<int> >()->multitoken(), "SOP terms")
        ("pos", po::value< vector<int> >()->multitoken(), "POS terms")
        ("dont-care", po::value< vector<int> >()->multitoken(), "Dont-Care terms")
        ("literals-count,l", po::value<int>(&literals_count)->required(), "Literals count")
        ("debug", po::value<bool>(&debug)->default_value(false), "Show debug information")
    ;

//    po::positional_options_description p;
//    p.add("input-file", -1);

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv).
              options(desc)
//              .positional(p)
              .run()
              , vm);

    if (vm.count("help")) {
        cout << desc << "\n";
        return 1;
    }
    
    po::notify(vm);

    Terms terms;
    Terms dontCareTerms;
    Terms primeTerms;

    if (vm.count("sop")) {
        BOOST_FOREACH (int term_val, vm["sop"].as< vector<int>  >())
        {
            Term* term = new Term(term_val, literals_count);
            terms.push_back(term);
        }
    }
    
    if (vm.count("pos")) {
        BOOST_FOREACH (int term_val, vm["pos"].as< vector<int>  >())
        {
            // calculate the SOP term for the POS term - 2^(literals) - 1 - minterm
            Term* term = new Term(pow(2, literals_count) - 1 - term_val, literals_count);
            terms.push_back(term);
        }
    }
    
    if (vm.count("dont-care")) {
        BOOST_FOREACH (int term_val, vm["dont-care"].as< vector<int>  >())
        {
            Term* term = new Term(term_val, literals_count);
            term->dontCare();
            terms.push_back(term);
            dontCareTerms.push_back(term);
        }
    }

    sort(terms.begin(), terms.end(), PointerCompare());

    queue<Term*> termsQueue;
    for (Terms::iterator it = terms.begin(); 
        it < terms.end(); ++it) 
    {
        termsQueue.push(*it);
    }

    termsQueue.push(0); // separating the algorithm steps
    
    Terms currentTerms;
    Terms nextTerms;
    while (!termsQueue.empty()) {
        // 1. go over the queue, while adding to current terms
        //    a. if 0 or different number of ones, stop adding.
        //    b. if 0, add unselected to primes.
        //    c. if number of ones is bigger by one, fetch until 0 or different number of ones.
        //    d. compare and push to queue.
        //    e. clear next and current.

        Term* first;
        Term* front;
        bool match_terms = true;
        bool find_next_terms = true;
        if (currentTerms.empty()) {
            // get the first term in the queue
            first = termsQueue.front();
            
            // get all the terms in the queue with the same amount of
            // ones in the binary representation
            while (!termsQueue.empty()) {
                front = termsQueue.front();
                
                if (front == 0 || first == 0) {
                    termsQueue.pop();
                    match_terms = false;
                    find_next_terms = false;
                    break;
                }
                
                if ((front->oneCount() - first->oneCount()) > 1) {
                    match_terms = false;
                    find_next_terms = false;
                    break;
                }

                if (front->oneCount() != first->oneCount()) {
                    // difference is exactly 1.
                    // we should start adding next terms.
                    break;
                }
                
                currentTerms.push_back(front);
                termsQueue.pop();
            }
        }

        // by now we have some currentTerms, and are ready to insert
        // to nextTerms.

        nextTerms.clear();
        
        if (find_next_terms && currentTerms.size() > 0) {
            // get the first term in the queue
            first = termsQueue.front();

            // get all the terms in the queue with the same amount of
            // ones in the binary representation
            while (!termsQueue.empty()) {
                front = termsQueue.front();

                if (front == 0 || first == 0) {
                    termsQueue.pop();
                    termsQueue.push(0);
                    break;
                }

                if (front->oneCount() != first->oneCount()) {
                    // we should start processing the current terms sets.
                    break;
                }

                nextTerms.push_back(front);
                termsQueue.pop();
            }
        }
        
        if (match_terms && currentTerms.size() > 0 && nextTerms.size() > 0) {
            // now we have two lists of terms - current and next, which
            // are adjacent groups in a Quine-McCluskey table.
            // we should compare each current term and next term and
            // push new terms to the end of the queue.
            for (Terms::iterator c_it = currentTerms.begin();
                 c_it < currentTerms.end(); ++c_it)
            {
                Term* current_term = *c_it;
                for (Terms::iterator n_it = nextTerms.begin();
                     n_it < nextTerms.end(); ++n_it)
                {
                    Term* next_term = *n_it;
                    int separating_bit = current_term->separatingBit(*next_term);
                    if (separating_bit > -1) {
                        current_term->select();
                        next_term->select();

                        Term* new_term = new Term(*current_term, 
                                                  *next_term, 
                                                  separating_bit+1);
                        termsQueue.push(new_term);
                        terms.push_back(new_term); // add to heap pointers vector
                    }
                }
            }

        }

        // if we got here, we should try to add unselected terms
        // from both current terms and next terms and then clear both lists.

        for (Terms::iterator it = currentTerms.begin();
             it < currentTerms.end(); ++it)
        {
            if (!(*it)->isSelected()) {
                // make sure that we don't add terms that are made of
                // only dont care terms.
                if ((*it)->isDontCare()) {
                    continue;
                }

                // make sure we don't have that term in the list already
                // (that can happen because 0,8-1,9 are equal to 0,1-8,9, for example)
                bool is_duplicate = false;

                for (Terms::iterator pt_it = primeTerms.begin(); 
                    pt_it < primeTerms.end(); ++pt_it) 
                {
                    if ((*pt_it)->getExpression() == (*it)->getExpression()) {
                        is_duplicate = true;
                        break;
                    }
                }

                if (!is_duplicate) {
                    primeTerms.push_back(*it);
                }
            }
        }

        currentTerms = nextTerms;
    }
    
//    cout << "pre-minimize: f = " << terms_function(primeTerms) << endl;

    Terms primeImplicants;
    ImplicantsMap implicantsMap;
    for (Terms::iterator it = primeTerms.begin(); 
        it < primeTerms.end(); ++it) 
    {
        Term* term = *it;
        term->primeImplicantCandidate();
        vector<size_t> minterms = term->getMinterms();
        for (vector<size_t>::iterator mt_it = minterms.begin();
             mt_it < minterms.end(); ++mt_it)
        {
            implicantsMap[*mt_it].push_back(term);
        }
    }

    // remove dontcare minterms from implicantsMap, as we don't need to
    // actually cover them.
    for (Terms::iterator it = dontCareTerms.begin();
         it < dontCareTerms.end(); ++it)
    {
        size_t minterm = (*it)->getDecimal();
        for (Terms::iterator t_it = implicantsMap[minterm].begin();
             t_it < implicantsMap[minterm].end(); ++t_it)
        {
            (*t_it)->coverMinterm(minterm);
        }

        implicantsMap[minterm].clear();
    }


    for (ImplicantsMap::iterator main_it = implicantsMap.begin();
         main_it != implicantsMap.end(); ++main_it)
    {
        // find minterms that are only covered by one implicant, and remove any
        // term from the map that cover the minterms that implicant does.
        bool found_prime_implicant = false;
        for (ImplicantsMap::iterator it = implicantsMap.begin();
             it != implicantsMap.end(); ++it)
        {
            if ((*it).second.size() == 1) {
                Term* term = (*it).second.at(0);
                
                // we found a new prime implicant!
//                cout << "New (natural) prime implicant for term " << (*it).first << "! " << term->getExpression() <<
//                        " (" << term->getDecimals() << ")" << endl;
//                cout << (*it).second << endl;

                term->primeImplicant();
                found_prime_implicant = true;

                primeImplicants.push_back(term);
                vector<size_t> minterms = term->getMinterms();
                for (vector<size_t>::iterator mt_it = minterms.begin();
                     mt_it < minterms.end(); ++mt_it)
                {
                    for (Terms::iterator t_it = implicantsMap[*mt_it].begin();
                         t_it < implicantsMap[*mt_it].end(); ++t_it)
                    {
                        (*t_it)->coverMinterm(*mt_it);
                    }

                    implicantsMap[*mt_it].clear();
                }
            }
        }

        if (!found_prime_implicant) {
            // choose an arbitrary implicant to be a prime implicant
            // -- we are choosing the first term we can find that appears the most
            sort(primeTerms.begin(), primeTerms.end(), PrimeImplicantCandidatesCompare());

            // find the first non-prime implicant term
            Term* term = 0;
            for (Terms::iterator pt_it = primeTerms.begin();
                 pt_it < primeTerms.end(); ++pt_it)
            {
                if (!(*pt_it)->isPrimeImplicant() 
                    && (*pt_it)->getRemainingMinterms().size() > 0) 
                {
                    term = *pt_it;
                    break;
                }
            }
            
            if (term == 0) {
                // we don't have any more prime terms! we are done.
                break;
            }

            // we found a new prime implicant!
//            cout << "New prime implicant! " << term->getExpression() <<
//                    " (" << term->getDecimals() << ")" << endl;
            term->primeImplicant();
            primeImplicants.push_back(term);

            vector<size_t> minterms = term->getMinterms();
            for (vector<size_t>::iterator mt_it = minterms.begin();
                 mt_it < minterms.end(); ++mt_it)
            {
                for (Terms::iterator t_it = implicantsMap[*mt_it].begin();
                     t_it < implicantsMap[*mt_it].end(); ++t_it)
                {
                    (*t_it)->coverMinterm(*mt_it);
                }

                implicantsMap[*mt_it].clear();
            }
        }
    }

    cout << "f = " << terms_function(primeImplicants, debug) << endl;
    
    // clean everything!
 
    for (Terms::iterator it = terms.begin(); 
         it < terms.end(); ++it) 
    {
        delete (*it);
        *it = 0;
    }
}
