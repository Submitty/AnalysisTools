import utils

class Context:
	def __init__(self, language, lookaheadTags=["\*"], siblingTags=["\*"], parentTags=["\*"], grandParentTags=["\*"]):
		self.language = language
		self.lookaheadTags = lookaheadTags
		self.siblingTags = siblingTags
		self.parentTags = parentTags
		self.grandParentTags = grandParentTags
		self.dummyLookahead = dict({"tags": self.lookaheadTags})
		self.dummyParent = dict({"tags": self.parentTags})
		self.dummyGP = dict({"tags": self.grandParentTags})
		self.dummySibling = dict({"tags": siblingTags})
		self.dummyNode = dict({"tags": "\*"})

	def __eq__(self, other):
		if not self.language == other.language: return False

		if(not self.lookaheadTags == None and not other.lookaheadTags == None and 
			(utils.tagsMatch(self.dummyLookahead, other.dummyLookahead, self.dummyNode, other.dummyNode, self.language, False) == -1)):
				return False
		elif((self.lookaheadTags == None) ^ (other.lookaheadTags == None) and not
			(self.lookaheadTags == ["\*"] or other.lookaheadTags == ["\*"])): return False

		if(not self.siblingTags == None and not other.siblingTags == None and 
			(utils.tagsMatch(self.dummySibling, other.dummySibling, self.dummyParent, other.dummyParent, self.language, False) == -1)):
				return False
		elif((self.siblingTags == None) ^ (other.siblingTags == None) and not
			(self.siblingTags == ["\*"] or other.siblingTags == ["\*"])): return False

		if (not self.parentTags == None and not other.parentTags == None and 
			(utils.tagsMatch(self.dummyParent, other.dummyParent, self.dummyGP, other.dummyGP, self.language, False) == -1)):
				return False
		elif((self.parentTags == None) ^ (other.parentTags == None) and not
			(self.parentTags == ["\*"] or other.parentTags == ["\*"])): return False

		if (not self.grandParentTags == None and not other.grandParentTags == None and 
			(utils.tagsMatch(self.dummyGP, other.dummyGP, None, None, self.language, False) == -1)):
				return False
		elif((self.grandParentTags == None) ^ (other.grandParentTags == None) and not
			(self.grandParentTags == ["\*"] or other.grandParentTags == ["\*"])): return False


		'''
		if self.lookaheadTags == ["\*"] or other.lookaheadTags == ["\*"]: return True
		if self.parentTags == ["\*"] or other.parentTags == ["\*"]: return True

		if self.lookaheadTags == None and not other.lookaheadTags == None: return False
		if not self.lookaheadTags == None and other.lookaheadTags == None: return False
		if self.siblingTags == None and not other.siblingTags == None: return False
		if not self.siblingTags == None and other.siblingTags == None: return False
		if self.parentTags == None and not other.parentTags == None: return False
		if not self.parentTags == None and other.parentTags == None: return False
		if self.grandParentTags == None and not other.grandParentTags == None: return False
		if not self.grandParentTags == None and other.grandParentTags == None: return False
		'''

		return True

	def __str__(self): 
		ret = ""
		if not self.lookaheadTags == None and len(self.lookaheadTags) > 0:
			ret += " lookahead: " + self.lookaheadTags[0]
		if not self.siblingTags == None and len(self.siblingTags) > 0:
			ret += " siblings: " + self.siblingTags[0]
		if not self.parentTags == None:
			ret += " parent: " + self.parentTags[0]
		if not self.grandParentTags == None:
			ret += " gp: " + self.grandParentTags[0]

		if ret == "":
			ret = "empty context"	
		
		return ret
