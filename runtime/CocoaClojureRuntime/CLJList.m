//
//  CLJList.m
//  CocoaClojureRuntime
//
//  Created by Justin Spahr-Summers on 2012-07-06.
//  Released into the public domain.
//

#import "CLJList.h"

@implementation CLJList

#pragma mark - Initialization

- (id)initWithValue:(id)value next:(CLJList *)next {
    self = [super init];
    if (!self)
        return nil;

    _value = value;
    _next = [next copy];

    return self;
}

#pragma mark - Collection operations

- (instancetype)conjoin:(id)value {
    return [[[self class] alloc] initWithValue:value next:self];
}

#pragma mark - NSCopying

- (id)copyWithZone:(NSZone *)zone {
    return self;
}

#pragma mark - NSObject

- (NSUInteger)hash {
    return [self.value hash];
}

- (BOOL)isEqual:(CLJList *)list {
    if (![list isKindOfClass:[CLJList class]])
        return NO;

    // TODO: handle nils
    if (![self.value isEqual:list.value])
        return NO;

    // TODO: handle nils
    return [self.next isEqual:list.next];
}

@end
