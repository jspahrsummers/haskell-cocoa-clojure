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

+ (id)listWithValues:(id)value, ... NS_REQUIRES_NIL_TERMINATION {
	NSMutableArray *values = [NSMutableArray array];
	va_list args;
	va_start(args, value);
	for(id currentValue = value; currentValue != nil; currentValue = va_arg(args, id)) {
		[values addObject:currentValue];
	}
	va_end(args);
	
	CLJList *last = nil;
	for(id currentValue in [values reverseObjectEnumerator]) {
		CLJList *list = [[self alloc] initWithValue:currentValue next:last];
		last = list;
	}
	
	return last;
}

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

- (NSString *)description {
	NSMutableString *valuesString = [NSMutableString stringWithString:@"("];
	
	CLJList *next = self;
	// Compare against value to handle the case of the empty list where self
	// would have a nil value.
	while(next.value != nil) {
		[valuesString appendFormat:@"%@", next.value];
		next = next.next;
		if(next != nil) [valuesString appendString:@", "];
	}
	
	[valuesString appendString:@")"];
	
	return valuesString;
}

@end
