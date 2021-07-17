use bitflags::*;

bitflags! {
    /// VarianceFlags ported from `tsc`.
    pub struct VarianceFlag: u32 {
        /// Neither covariant nor contravariant
        const INVARIANT = 0;
        const COVARIANT = 1 << 0;
        const CONTRAVARIANT = 1 << 1;
        /// Both covariant and contravariant
        const BIVARIANT = Self::COVARIANT.bits | Self::CONTRAVARIANT.bits;

        const INDEPENDENT = 1 << 2;

        /// Mask containing all measured variances without the unmeasurable flag
        const VARIANCE_MASK = Self::COVARIANT.bits | Self::CONTRAVARIANT.bits | Self::INDEPENDENT.bits;

        /// Variance result is unusable - relationship relies on structural comparisons which are not reflected in generic relationships
        const UNMEASURABLE = 1 << 3;  
        /// Variance result is unreliable - checking may produce false negatives, but not false positives
        const UNRELIABLE = 1 << 4;  

        const ALLOWS_STURUCTURAL_FALLBACK = Self::UNMEASURABLE.bits | Self::UNRELIABLE.bits;
    }
}
