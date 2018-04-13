;;;========================================================
;;; Sistema Experto Deportistas:
;;;
;;; Es un sistema experto que hace preguntas y en base a 
;;; los datos determina que personaje del deporte mundial 
;;; es y dice sus logros.
;;;
;;; Asignatura: Inteligencia Artificial
;;;
;;; Hecho por: Eddy Fidel, Ricardo De la Rosa, 
;;;         Maicel Abreu y Wilfredo Hernández
;;;========================================================


;;;========================================================
;;; Funciones
;;;========================================================


(deffunction preguntar (?interrogante $?valores-permitidos)
   (printout t ?interrogante)
   (bind ?respuesta (read))
   (if (lexemep ?respuesta) 
       then (bind ?respuesta (lowcase ?respuesta)))
   (while (not (member ?respuesta ?valores-permitidos)) do
      (printout t ?interrogante)
      (bind ?respuesta (read))
      (if (lexemep ?respuesta) 
          then (bind ?respuesta (lowcase ?respuesta))))
   ?respuesta)

(deffunction responder-si-o-no (?interrogante)
   (bind ?respuesta (preguntar ?interrogante si no s n))
   (if (or (eq ?respuesta si) (eq ?respuesta s))
       then si 
       else no))


;;;========================================================
;;; Reglas generales
;;;========================================================


(defrule determinar-deporte ""
   (not (deporte ?))
   (not (jugador ?))
   =>
   (assert (deporte
      (preguntar "¿Cuál es el deporte que desea consultar? (baloncesto / atletismo / beisbol) "
                    baloncesto atletismo beisbol))))


;;;========================================================
;;; Reglas de preguntas para baloncesto
;;;========================================================


(defrule determine-tyre-state ""
   (deporte atletismo)
   (not (tyre-inflated ?))
   (not (jugador ?))
   =>
   (assert (atletismo-inflated (responder-si-o-no "Are the atletismo inflated (si/no)? "))))
   
(defrule determine-check-puncture ""
   (deporte atletismo)
   (atletismo-inflated no)
   (not (jugador ?))
   =>
   (assert (atletismo-puncture (responder-si-o-no "Are the atletismo punctured (si/no)? "))))
   
(defrule determine-check-alignment ""
   (deporte atletismo)
   (atletismo-inflated si)
   (not (jugador ?))
   =>
   (assert (atletismo-alignment (responder-si-o-no "Are the atletismo aligned (si/no)? "))))
   
(defrule determine-check-movement ""
   (deporte atletismo)
   (atletismo-inflated si)
   (atletismo-alignment si)
   (not (jugador ?))
   =>
   (assert (atletismo-movement (responder-si-o-no "Are the atletismo moving freely (si/no)? "))))
   
(defrule determine-check-vibration ""
   (deporte atletismo)
   (atletismo-inflated si)
   (atletismo-alignment si)
   (atletismo-movement si)
   (not (jugador ?))
   =>
   (assert (atletismo-vibration (responder-si-o-no "Are the atletismo vibrating (si/no)? "))))
   
(defrule determine-check-mud ""
   (deporte atletismo)
   (atletismo-inflated si)
   (atletismo-alignment si)
   (atletismo-movement si)
   (atletismo-vibration si)
   (not (jugador ?))
   =>
   (assert (atletismo-mud-present (responder-si-o-no "Is mud or dirt present in rims (si/no)? "))))
   
(defrule determine-check-tyre-noise ""
   (deporte atletismo)
   (atletismo-inflated si)
   (atletismo-alignment si)
   (atletismo-movement si)
   (atletismo-vibration si)
   (atletismo-mud-present no)
   (not (jugador ?))
   =>
   (assert (atletismo-noise-present (responder-si-o-no "Is there excessive play or noise from the wheel bearings (si/no)? "))))


;;;========================================================
;;; Reglas del jugador y logros para baloncesto
;;;========================================================


(defrule normal-tyre-state-conclusions ""
   (deporte atletismo)
   (atletismo-inflated si)
   (atletismo-alignment si)
   (atletismo-movement si)
   (atletismo-vibration no)
   (not (jugador ?))
   =>
   (assert (jugador "No jugador needed.")))

(defrule atletismo-punctured-si ""
   (deporte atletismo)
   (atletismo-inflated no)
   (atletismo-puncture si)
   (not (jugador ?))
   =>
   (assert (jugador "Get Puncture jugadored.")))
   
(defrule atletismo-punctured-no ""
   (deporte atletismo)
   (atletismo-inflated si)
   (atletismo-puncture no)
   (not (jugador ?))
   =>
   (assert (jugador "Get atletismo Inflated.")))

(defrule atletismo-aligned-no ""
   (deporte atletismo)
   (atletismo-inflated si)
   (atletismo-alignment no)
   (not (jugador ?))
   =>
   (assert (jugador "Get atletismo Aligned.")))
   
(defrule atletismo-movement-no ""
   (deporte atletismo)
   (atletismo-movement no)
   (not (jugador ?))
   =>
   (assert (jugador "Apply oil in the axle.")))
   
(defrule atletismo-mud-si ""
   (deporte atletismo)
   (atletismo-mud-present si)
   (not (jugador ?))
   =>
   (assert (jugador "Clean mud or dirt packed in the back of the rim.")))
   
(defrule atletismo-noise-si ""
   (deporte atletismo)
   (atletismo-noise-present si)
   (not (jugador ?))
   =>
   (assert (jugador "Change Loose, worn or damaged wheel bearings.")))


;;;========================================================
;;; Reglas conclusión
;;;========================================================


(defrule cabecera ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "Sistema Experto Deportistas")
  (printout t crlf crlf))

(defrule imprimir-jugador ""
  (declare (salience 10))
  (jugador ?texto)
  =>
  (printout t crlf crlf)
  (printout t "Jugador y sus logros:")
  (printout t crlf crlf)
  (format t "%s%n%n%n" ?texto))

