use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub struct LineMap {
    map: BTreeMap<u32, u32>,
}

impl LineMap {
    /// Construct a new, empty LineMap
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new line boundary
    ///
    /// # Parameters
    ///
    /// * `offset`: starting offset of the next line in the input string
    pub fn add_line(&mut self, offset: u32) {
        self.map.insert(offset, self.map.len() as _);
    }

    /// Split an offset into line and column information
    ///
    /// # Parameters
    ///
    /// * `offset`: offset to split
    pub fn get_line_and_col(&self, offset: u32) -> (u32, u32) {
        // unwrap: offset >= 0 so there will always be a matching entry
        let (prev_start_offset, line) = self.map.range(..=offset).next_back().unwrap();
        (*line, offset - prev_start_offset)
    }
}

impl Default for LineMap {
    fn default() -> Self {
        Self {
            map: {
                let mut map = BTreeMap::new();
                map.insert(0, 0);
                map
            },
        }
    }
}

impl lang_util::located::Resolver for LineMap {
    fn resolve(&self, offset: lang_util::TextSize) -> (u32, u32) {
        self.get_line_and_col(offset.into())
    }
}
