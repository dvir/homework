#ifndef LINKED_LIST_H_
#define LINKED_LIST_H_
#include <string>

class Link {
/**
 * The linked list is made of Link objects
 * each Link holds a pointer to the next list and its data (a string)
 * The Link object only knows about a single link - it never iterates through the next
 * No deep copy or deep delete - these are the responsibility of the LinkedList.
 */
  private:
    Link* next_;
    std::string data_;

   public:
    Link();
    Link(const std::string& data,  Link* link);
    Link(const Link& aLink);
    virtual ~Link();
    void setNext(Link* link);
    Link* getNext() const;
    const std::string& getData() const;
};

class List {
   private:
    Link* head_;
    Link* copy() const;
    void clear();

   public:
    List(); // constructor

    const Link* getHead() const;
    void insertData(const std::string& data);
    void removeFirst();

    List(const List& aList);
    virtual ~List();
    List& operator=(const List &L);
};
#endif /*LINKED_LIST_H_*/
