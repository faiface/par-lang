use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use lsp_types::Uri;

#[derive(Clone)]
pub struct IO {
    virtual_files: VirtualFiles,
}

impl IO {
    pub fn new() -> IO {
        Self {
            virtual_files: VirtualFiles::new(),
        }
    }

    pub fn update_file(&mut self, uri: &Uri, content: String) {
        self.virtual_files.update_file(uri, content);
    }

    pub fn read(&self, uri: &Uri) -> Option<String> {
        self.virtual_files.read(uri)
    }
}

// not thread safe (is this ok?)
#[derive(Clone)]
struct VirtualFiles {
    files: Rc<RefCell<HashMap<Uri, String>>>,
}

impl VirtualFiles {
    fn new() -> VirtualFiles {
        Self {
            files: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    fn update_file(&mut self, uri: &Uri, content: String) {
        self.files.borrow_mut().insert(uri.clone(), content);
    }

    fn read(&self, uri: &Uri) -> Option<String> {
        self.files.borrow().get(uri).cloned()
    }
}