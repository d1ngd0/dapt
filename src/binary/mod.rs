use byteorder::BigEndian;

pub struct Binary(Vec<u8>);

// BToken is the most generic token within dapt. All tokens
// in dapt are btokens except breferences. btokens consist of
// 0x0, 0x0 u16 length
// 0x0 u8 type
// 0x0, 0x0, 0x0, 0x0 u32 breference offset
// 0x0, 0x0, 0x0, 0x0 u32 parent offset
// if the parent offset is 0 it is assumed to be
// unset. a reference to the root of the document (as a breference)
// will be at 0, so should never be a valid parent reference.
pub struct BToken(&[u8]);

static LENGTH_OFFSET: usize = 0;
static LENGTH_OFFSET_END: usize = LENGTH_OFFSET + 2;

static TYPE_OFFSET: usize = LENGTH_OFFSET_END;
static TYPE_OFFSET_END: usize = TYPE_OFFSET + 1;

static REFERENCE_OFFSET: usize = TYPE_OFFSET_END;
static REFERENCE_OFFSET_END: usize = REFERENCE_OFFSET + 4;

static PARENT_OFFSET: usize = REFERENCE_OFFSET_END;
static PARENT_OFFSET_END: usize = PARENT_OFFSET + 4;

static CONTENT_OFFSET: usize = PARENT_OFFSET_END;

impl BToken {
    pub fn get_parent_index(&self) -> Option<u32> {
        let b = self.0.get(PARENT_OFFSET..PARENT_OFFSET_END)?;
        BigEndian::read_u32(b)
    }
}
