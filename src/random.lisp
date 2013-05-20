
(in-package :guicho-utilities)
(use-syntax :annot)

@export
@doc "gaussian random algorithm with box-muller method."
(defun gaussian-drandom (&optional (μ1 0.0d0) (σ1 1.0d0) μ2 σ2)
  (let* ((x (drandom 1.0d0))
		 (y (drandom 1.0d0))
		 (_x (dsqrt (d- (d* 2.0d0 (dlog x))))))
	(values (d+ μ1 (d* σ1 _x (dcos (d* +2pi+ y))))
			(d+ (or μ2 μ1) (d* (or σ2 σ1) _x (dsin (d* +2pi+ y)))))))

