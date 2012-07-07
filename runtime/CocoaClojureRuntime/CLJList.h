//
//  CLJList.h
//  CocoaClojureRuntime
//
//  Created by Justin Spahr-Summers on 2012-07-06.
//  Released into the public domain.
//

#import <Foundation/Foundation.h>

/**
 * Represents an immutable, singly-linked Clojure list.
 */
@interface CLJList : NSObject <NSCopying>

/**
 * The value stored at this node.
 */
@property (nonatomic, strong, readonly) id value;

/**
 * The next link in the list, or nil if this is the tail of the list.
 */
@property (nonatomic, copy, readonly) CLJList *next;

/**
 * Creates and returns a new list node.
 */
- (id)initWithValue:(id)value next:(CLJList *)next;

/**
 * Returns a new list with the given value at the front.
 */
- (instancetype)conjoin:(id)value;

@end
