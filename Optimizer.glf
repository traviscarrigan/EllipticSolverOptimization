# =============================================================================
# STRUCTURED DOMAIN OPTIMIZATION SCRIPT - OPTIMIZES MAXIMUM INCLUDED ANGLE
# USING VARIOUS INTEGER BASED OPTIMIZATION ALGORITHMS
# =============================================================================
# Written by Travis Carrigan
# 
# v1: Oct 18, 2011
# v2: Oct 24, 2011
# v3: Oct 31, 2011
# v4: Nov 02, 2011
# v5: Nov 14, 2011
#

# Load Glyph
package require PWI_Glyph 2.4

# Setup Pointwise and define working directory
pw::Application reset
pw::Application clearModified
set scriptDir [file dirname [info script]]


# -------------------------------------
# User Defined Parameters
# -------------------------------------
# Files to load
set projectFile "grid.pw"; # Project file
pw::Application load [file join $scriptDir $projectFile]
pw::Display update

# Entities
#set domList [list "dom-1"]; # Domain to optimize
#set domList [list "dom-2"]; # Domain to optimize
set domList [list "dom-1" "dom-2"]; # Domain to optimize

# Optimization setttings
set upBnd  50;  # Maximum solver iterations
set absTol 2.0; # Absolute tolerance (min=2)

set D       4;  # Number of parameters
set maxGen 20;  # Maximum number of generations
set NP     30;  # Number of population members
set F      0.8; # Amplification factor [0,2]
set CR     0.6; # Crossover constant [0,1]

set par(0,lowerBnd) 0  
set par(0,upperBnd) $upBnd
set par(1,lowerBnd) 0
set par(1,upperBnd) 2
set par(2,lowerBnd) 0
set par(2,upperBnd) 2
set par(3,lowerBnd) 0
set par(3,upperBnd) 3

# Optimization method
#set method "ExhaustiveSearch"; set plot "NO"
#set method "GoldenSection"
set method "DifferentialEvolution"
    #set strategy "DE/rand/1/bin"
    set strategy "DE/best/2/bin"



###############################################################
#-- PROC: GetMaxAngle
#--
#-- Get maximum included angle for a domain.
#--
###############################################################
proc GetMaxAngle {dom} {

    set examine [pw::Examine create DomainMaximumAngle]

        $examine addEntity $dom
        $examine examine 

    return [$examine getMaximum] 

}



###############################################################
#-- PROC: RunSolver
#--
#-- Run elliptic solver.
#--
###############################################################
proc RunSolver {dom iter} {

    set solverMode [pw::Application begin EllipticSolver $dom]

        $solverMode run $iter

    $solverMode end

    return $dom

}



###############################################################
#-- PROC: FuncEval
#--
#-- Objective function evaluation.
#--
###############################################################
proc FuncEval {dom params {reInit 1}} {

    set iter [lindex $params 0]

    if {[llength $params]>1} {

        set bnd  [lindex $params 1]
        set int  [lindex $params 2]
        set ang  [lindex $params 3] 

        set bndControl [list "StegerSorenson" "HilgenstockWhite" "None"]
        set intControl [list "ThomasMiddlecoff" "Laplace" "Fixed"]
        set angControl [list "Orthogonal" "Interpolate" "Current" "Adjacent"]

        $dom setEllipticSolverAttribute EdgeControl [lindex $bndControl $bnd]
        $dom setEllipticSolverAttribute InteriorControl [lindex $intControl $int]
        $dom setEllipticSolverAttribute EdgeAngleCalculation [lindex $angControl $ang]
    
    }

    set maxAngle [GetMaxAngle [RunSolver $dom $iter]]

    if {$reInit} {
        $dom initialize
    }
    
    return $maxAngle

}



###############################################################
#-- PROC: 2PtCon
#--
#-- Create a simple connector from two points.
#--
###############################################################
proc 2PtCon {pt1 pt2} {

    set conMode [pw::Application begin Create]
    
        set seg [pw::SegmentSpline create]
            $seg addPoint $pt1
            $seg addPoint $pt2
    
        set con [pw::Connector create]
            $con addSegment $seg
   
    $conMode end

    return $con

}



###############################################################
#-- PROC: ExhaustiveSearch
#--
#-- Optimize grid quality using Exhaustive Search technique.
#--
###############################################################	
if {$method=="ExhaustiveSearch"} {

    set domNum 0
    foreach ent $domList {

		# Start time
		set tBegin [pwu::Time now]
		
        # Get actual domain
        set dom [pw::Entity getByName $ent]

		# Retrieve maximum angle of each domain
		set origF [GetMaxAngle $dom]

        # Store original values
        set x 0
        set f $origF


		# -------------------------------------
		# Exhaustive Search Technique
		# -------------------------------------
        set minF $origF
        for {set i 1} {$i<=$upBnd} {incr i} {

            # Solver iterations
            lappend x $i

            # Max included angle
            lappend f [FuncEval $dom $i]

            # Store minimum included angle and iteration
            if {[lindex $f $i]<$minF} {
                
                set iters $i
                set minF [lindex $f $i]

            }
            
        }

		# Run elliptic solver
		RunSolver $dom $iters
	
	
		# -------------------------------------
	    # PRINT RESULTS
	    # -------------------------------------
	    puts ""
		puts "Optimization Results for Domain [$dom getName]"
		puts "----------------------------------------------"
        puts "Method: Exhaustive Search Technique"
        puts ""
		puts "Function evalutations: $upBnd"
		puts "Solver iterations:     $iters"
		puts ""
		puts [format "Original max included angle:  %.2f deg" $origF]
		puts [format "Optimized max included angle: %.2f deg" $minF]
		puts [format "Max angle improvement:        %.2f%%" [expr {abs($minF-$origF)/$origF*100.0}]]
		puts ""
		puts [format "Total run time: %.2f sec" [pwu::Time elapsed $tBegin]] 
		puts ""


		# -------------------------------------
	    # PLOT RESULTS
	    # -------------------------------------
        if {$plot=="YES"} {
        
            # Delete domains
            $dom delete -force -connectors

            # Scaling factor and offset distance
            set scaling [expr {180.0/$upBnd}]
            set offset 270

            # Create X-Y axis
            2PtCon "[expr {$domNum*$offset}] 90 0" "[expr {$domNum*$offset+180}] 90 0"
            2PtCon "[expr {$domNum*$offset}] 90 0" "[expr {$domNum*$offset}] 180 0"

            # Generate curve
            for {set i 0} {$i<$upBnd} {incr i} {

                set pt1 "[expr {$domNum*$offset+$scaling*[lindex $x $i]}] [lindex $f $i] 0"
                set pt2 "[expr {$domNum*$offset+$scaling*[lindex $x $i+1]}] [lindex $f $i+1] 0"

                2PtCon $pt1 $pt2

            }

            # Mark minimum using a pole connector
            set pt1 "[expr {$domNum*$offset+$scaling*$iters}] $minF 0"
            set pt2 "[expr {$domNum*$offset+$scaling*$iters}] $minF 0"
            
            2PtCon $pt1 $pt2

        }

    incr domNum

    }



###############################################################
#-- PROC: GoldenSection
#--
#-- Optimize grid quality using the Golden Section method.
#--
###############################################################
} elseif {$method=="GoldenSection"} {

    foreach ent $domList {

		# Start time
		set tBegin [pwu::Time now]
		
        # Get actual domain
        set dom [pw::Entity getByName $ent]

		# Retrieve maximum angle of each domain
		set origF [GetMaxAngle $dom]
		
		
		# -------------------------------------
		# Golden Section Method
		# -------------------------------------
		# Calculate the maximum number of function evaluations
		set xu $upBnd
		set xl 0
		set tau 0.38197
		set relTol [expr {double($absTol)/($xu-$xl)}]
		set n [expr {round(log($relTol)/log(1-$tau)+3)}]
		
		# Initialization
		set fl [GetMaxAngle $dom]
		set fu [FuncEval $dom $xu]
		set k 3
		
		set x1 [expr {round((1-$tau)*$xl+$tau*$xu)}]
		set f1 [FuncEval $dom $x1]
		
		set x2 [expr {round($tau*$xl+(1-$tau)*$xu)}]
		set f2 [FuncEval $dom $x2]
		 
		# Iterate to minimize maximum included angle
		while {$k<$n} {
		
		    if {$f1>$f2} {
		        
		        set xl $x1
		        set fl $f1
		        set x1 $x2
		        set f1 $f2
		
		        set x2 [expr {round($tau*$xl+(1-$tau)*$xu)}]
		        set f2 [FuncEval $dom $x2]
	
		    } else {
		
		        set xu $x2
		        set fu $f2
		        set x2 $x1
		        set f2 $f1
		
		        set x1 [expr {round((1-$tau)*$xl+$tau*$xu)}]
		        set f1 [FuncEval $dom $x1]
	
		    }
		
		    set k [expr {$k+1}]
	
		}
		
		# Determine minimum included angle
		set minF [expr {min($f1,$f2)}]
		
		if {$minF==$f1} {
		    set iters $x1
		} else {
		    set iters $x2
		}
		
		# Run elliptic solver
		RunSolver $dom $iters
	
	
		# -------------------------------------
	    # PRINT RESULTS
	    # -------------------------------------
	    puts ""
		puts "Optimization Results for Domain [$dom getName]"
		puts "----------------------------------------------"
        puts "Method: Golden Section Method"
        puts ""
		puts "Function evalutations: $k"
		puts "Solver iterations:     $iters"
		puts ""
		puts [format "Original max included angle:  %.2f deg" $origF]
		puts [format "Optimized max included angle: %.2f deg" $minF]
		puts [format "Max angle improvement:        %.2f%%" [expr {abs($minF-$origF)/$origF*100.0}]]
		puts ""
		puts [format "Total run time: %.2f sec" [pwu::Time elapsed $tBegin]] 
		puts ""

    }



###############################################################
#-- PROC: DifferentialEvolution
#--
#-- Optimize grid quality using Differential Evolution.
#--
###############################################################	
} elseif {$method=="DifferentialEvolution"} {

    set domNum 0
    foreach ent $domList {

		# Start time
		set tBegin [pwu::Time now]
		
        # Get actual domain
        set dom [pw::Entity getByName $ent]

		# Retrieve maximum angle of each domain
		set origF [GetMaxAngle $dom]

        # Initialization
        set count 0
        set a 0
        set b 0
        set c 0
        set d 0


		# -------------------------------------
		# Initialize Population
		# -------------------------------------
        # Random initial population
        for {set i 0} {$i<$NP} {incr i} {

            set params ""
            for {set j 0} {$j<$D} {incr j} {

                set x($count,$i,$j) [expr {round(rand()*$par($j,upperBnd))}]
                    
                if {$x($count,$i,$j)<$par($j,lowerBnd)} {
                    set x($count,$i,$j) $par($j,lowerBnd)
                }

                lappend params $x($count,$i,$j)

            }

            if {$x($count,$i,0)==0} {
                set cost($count,$i) $origF
            } else {
                set cost($count,$i) [FuncEval $dom $params]
            }

        }


		# -------------------------------------
		# Differential Evolution Algorithm
		# -------------------------------------
        # Continue until max generations is reached
        while {$count<$maxGen} {

            # Retrieve current best performing population member
            set costBest 180
            for {set i 0} {$i<$NP} {incr i} {

                if {$cost($count,$i)<$costBest} {

                    set costBest $cost($count,$i)
                    set popMem $i

                }

            }

            for {set i 0} {$i<$NP} {incr i} {

                for {set j 0} {$j<$D} {incr j} {
                    set xBest($count,$i,$j) $x($count,$popMem,$j)
                }

            }

            # Loop through population
            for {set i 0} {$i<$NP} {incr i} {

                # Randomly pick three vectors
                while {$a==$i} {
                    set a [expr {round(rand()*($NP-1))}]
                }

                while {$b==$i || $b==$a} {
                    set b [expr {round(rand()*($NP-1))}]
                }

                while {$c==$i || $c==$a || $c==$b} {
                    set c [expr {round(rand()*($NP-1))}]
                }

                while {$d==$i || $d==$a || $d==$b || $d==$c} {
                    set d [expr {round(rand()*($NP-1))}]
                }

                # Generate trial vector
                set j [expr {round(rand()*($D-1))}]

                for {set k 1} {$k<=$D} {incr k} {

                    if {[expr {rand()}]<$CR || $k==$D} {

                        if {$strategy=="DE/rand/1/bin"} {
                            set trial($j) [expr {round($x($count,$c,$j)+$F*($x($count,$a,$j)-$x($count,$b,$j)))}]
                        } elseif {$strategy=="DE/best/2/bin"} {
                            set trial($j) [expr {round($xBest($count,$i,$j)+$F*($x($count,$a,$j)+$x($count,$b,$j)- \
                                                       $x($count,$c,$j)-$x($count,$d,$j)))}]
                        }

                    } else {
                        set trial($j) $x($count,$i,$j)
                    }

                    set j [expr {round(fmod(($j+1),$D))}]

                }

                # Penalty - bring solution back to boundary
                set trialList ""
                for {set j 0} {$j<$D} {incr j} {

                    if {$trial($j)>$par($j,upperBnd)} {
                        set trial($j) $par($j,upperBnd)
                    } elseif {$trial($j)<$par($j,lowerBnd)} {
                        set trial($j) $par($j,lowerBnd)
                    }

                    lappend trialList $trial($j)             
 
                }

                # Evaluate objective function
                if {$trial(0)==0} {
                    set score $origF
                } else {
                    set score [FuncEval $dom $trialList]
                }

                # Selection operation
                if {$score<=$cost($count,$i)} {

                    for {set j 0} {$j<$D} {incr j} {

                        set x([expr {$count+1}],$i,$j) $trial($j)
                        set cost([expr {$count+1}],$i) $score

                    } 
                
                } else {
    
                    for {set j 0} {$j<$D} {incr j} {

                        set x([expr {$count+1}],$i,$j) $x($count,$i,$j)
                        set cost([expr {$count+1}],$i) $cost($count,$i)

                    }
                
                }
            
            } 

            incr count     

        }


		# -------------------------------------
		# Select Optimized Solution
		# -------------------------------------
        # Pick best result from all generations
        set count 0
        set minF $origF

        while {$count<$maxGen} {
        
            for {set i 0} {$i<$NP} {incr i} {

                if {$cost($count,$i)<$minF} {

                    set minF $cost($count,$i)
                    set genF $count

                    set minParams ""
                    for {set j 0} {$j<$D} {incr j} {
                        lappend minParams $x($count,$i,$j)
                    }

                }

            }
        
            incr count

        }

		# Run elliptic solver
        FuncEval $dom $minParams 0

	    # -------------------------------------
	    # Calculate Variance
	    # -------------------------------------
        set count 0
        while {$count<$maxGen} {

            set mu($count)  0
            set var($count) 0
        
            # Calculate expected value
            for {set i 0} {$i<$NP} {incr i} {
                set mu($count) [expr {(1.0/$NP)*$cost($count,$i)+$mu($count)}]
            }

            # Calculate expected squared deviation
            for {set i 0} {$i<$NP} {incr i} {
                set var($count) [expr {(1.0/$NP)*pow($cost($count,$i)-$mu($count),2)+$var($count)}]
            }        

            puts "Generation $count variance: $var($count)"

            incr count

        }
	
	
		# -------------------------------------
	    # PRINT RESULTS
	    # -------------------------------------
	    puts ""
		puts "Optimization Results for Domain [$dom getName]"
		puts "----------------------------------------------"
        puts "Method: Differential Evolution"
        puts ""
		puts "Function evalutations:     [expr {$maxGen*$NP}]"
        puts "Generation optimum found:  $genF"
		puts "Solver iterations:         [lindex $minParams 0]"
        puts "Final generation variance: $var([expr {$maxGen-1}])"
		puts ""
		puts [format "Original max included angle:  %.2f deg" $origF]
		puts [format "Optimized max included angle: %.2f deg" $minF]
		puts [format "Max angle improvement:        %.2f%%" [expr {abs($minF-$origF)/$origF*100.0}]]
		puts ""
		puts [format "Total run time: %.2f sec" [pwu::Time elapsed $tBegin]] 
		puts ""

    incr domNum

    }

}



# 
# END SCRIPT
#
