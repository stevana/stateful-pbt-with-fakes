#ifndef QUEUE_H
#define QUEUE_H

typedef struct queue {
  int *buf;
  int inp, outp, size;
} Queue;

Queue *new(int n);
Queue *newBroken(int n);
void put(Queue *q, int n);
int get(Queue *q);
int size(Queue *q);
int sizeBroken(Queue *q);
int sizeBroken2(Queue *q);

#endif
