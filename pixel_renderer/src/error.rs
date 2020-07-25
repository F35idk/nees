use ash::{self, vk};
use std::error;
use std::fmt;
use std::io;

#[derive(Debug)]
pub enum VulkanError {
    Loading(ash::LoadingError),
    Instance(ash::InstanceError),
    Result(vk::Result),
    Io(io::Error),
    NoMemoryType(&'static str),
    NoPresentMode(&'static str),
    NoFormat(&'static str),
}

impl fmt::Display for VulkanError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VulkanError::Loading(e) => e.fmt(f),
            VulkanError::Instance(e) => e.fmt(f),
            VulkanError::Result(e) => e.fmt(f),
            VulkanError::Io(e) => e.fmt(f),
            VulkanError::NoMemoryType(s) => f.write_str(s),
            VulkanError::NoPresentMode(s) => f.write_str(s),
            VulkanError::NoFormat(s) => f.write_str(s),
        }
    }
}

impl error::Error for VulkanError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            VulkanError::Loading(e) => Some(e),
            VulkanError::Instance(e) => Some(e),
            VulkanError::Result(e) => Some(e),
            VulkanError::Io(e) => Some(e),
            VulkanError::NoMemoryType(_) => None,
            VulkanError::NoPresentMode(_) => None,
            VulkanError::NoFormat(_) => None,
        }
    }
}

impl From<ash::LoadingError> for VulkanError {
    fn from(err: ash::LoadingError) -> Self {
        VulkanError::Loading(err)
    }
}

impl From<ash::InstanceError> for VulkanError {
    fn from(err: ash::InstanceError) -> Self {
        VulkanError::Instance(err)
    }
}

impl From<vk::Result> for VulkanError {
    fn from(err: vk::Result) -> Self {
        VulkanError::Result(err)
    }
}

impl From<io::Error> for VulkanError {
    fn from(err: io::Error) -> Self {
        VulkanError::Io(err)
    }
}
